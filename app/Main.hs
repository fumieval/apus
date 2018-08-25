{-# LANGUAGE OverloadedLabels, ScopedTypeVariables, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import RIO
import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Extra
import Control.Lens (lastOf, folded)
import Control.Monad.Trans.Cont
import Data.Aeson as J
import qualified Data.ByteString.Base64 as Base64
import Data.Time.Clock
import Data.Winery
import qualified Data.Sequence as Seq
import qualified Database.Liszt as L
import qualified Database.Liszt.Internal as L
import GHC.Generics (Generic)
import GHC.IO.Encoding
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.Wai as Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.WebSockets
import Network.Wai.Middleware.Static
import Network.WebSockets as WS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import qualified RIO.HashMap as HM
import System.Environment

import Auth.GitHub
import Auth
import Types

data ApusReq = Submit !Text
  | Token !(Maybe AccessToken)
  deriving Generic
instance FromJSON ApusReq
instance ToJSON ApusReq

data ApusResp = Content ![Revision] !Text
  | AuthAck !Text
  | RecentChanges (Seq.Seq (Text, Text, Text))
  deriving Generic
instance FromJSON ApusResp
instance ToJSON ApusResp

broadcastChanges :: Global -> IO ()
broadcastChanges Global{..} = do
  changes <- atomically $ readTVar vRecentChanges
  envs <- atomically $ readTVar vEnvs
  forM_ envs $ \env -> do
    clients <- atomically $ readTVar $ vClients env
    forM_ clients
      $ \conn -> sendTextData conn $ J.encode $ RecentChanges changes

updateArticle :: Text -> Env -> Int -> Text -> STM (IO ())
updateArticle name Env{..} authorId theirs = do
  clientInfo <- readTVar vClientInfo
  authorName <- case IM.lookup authorId clientInfo of
    Just s -> return s
    Nothing -> fail "Unauthorised"
  m <- readTVar vClients

  modifyTVar (vRecentChanges global)
    $ \s -> (Seq.|> (name, T.takeWhile (/='\n') theirs, authorName))
    $ if length s >= recentChangesCount (config global) then Seq.drop 1 s else s

  return $ runRIO Env{..} $ do
    now <- liftIO getCurrentTime
    liftIO $ L.commit (storage global) $ L.insertTagged
      ("A" <> encodeUtf8 name)
      (toEncodingWithSchema Revision
        { revTime = now
        , revAuthor = authorName
        })
      theirs
    logInfo $ display authorName <> " updated " <> display name
    forM_ m $ \conn -> do
      revs <- liftIO $ getRevisions global name
      liftIO $ sendTextData conn $ J.encode $ Content (map fst revs) theirs
      `catch` \(e :: SomeException) -> logError $ display e

handleRequest :: Text -> Env -> WS.Connection -> Int -> ApusReq -> IO ()
handleRequest name Env{..} conn clientId = \case
  Submit doc -> do
    join $ atomically $ updateArticle name Env{..} clientId doc
    broadcastChanges global
  Token (Just tok) -> join $ atomically $ do
    users <- readTVar vUserInfo
    case HM.lookup tok users of
      Just s -> do
        modifyTVar vClientInfo $ IM.insert clientId s
        return $ sendTextData conn $ J.encode $ AuthAck s
      Nothing -> return $ pure ()
  Token Nothing -> atomically $ modifyTVar vClientInfo $ IM.delete clientId
  where
    Global{..} = global

serverApp :: Text -> Env -> WS.ServerApp
serverApp name Env{..} pending = do
  let Global{..} = global
  conn <- acceptRequest pending
  forkPingThread conn 10
  join $ atomically $ do
    i <- readTVar vFreshClientId
    writeTVar vFreshClientId $! i + 1
    modifyTVar vClients $ IM.insert i conn
    return $ do
      revs <- getRevisions global name
      initialContent <- case lastOf folded revs of
        Nothing -> pure ""
        Just (_, rp) -> T.decodeUtf8 <$> L.fetchPayload storage rp
      liftIO $ sendTextData conn $ J.encode $ Content (map fst revs) initialContent
      changes <- atomically $ readTVar vRecentChanges
      liftIO $ sendTextData conn $ J.encode $ RecentChanges changes
      forever $ do
        msg <- WS.receiveData conn
        case decode msg of
          Nothing -> WS.sendClose conn ("Invalid message" :: Text)
          Just req -> handleRequest name Env{..} conn i req

      `finally` atomically (do
        modifyTVar vClients $ IM.delete i
        modifyTVar vClientInfo $ IM.delete i)
      `catch` \(_ :: ConnectionException) -> return ()

multiServer :: Global -> WS.ServerApp
multiServer global@Global{..} pending = do
  envs <- atomically $ readTVar vEnvs
  env <- case HM.lookup name envs of
    Just env -> return env
    Nothing -> createEnv global name
  serverApp name env pending
  where
    name = T.decodeUtf8 $ B.drop 1 $ requestPath $ pendingRequest pending

getRevisions :: Global -> Text -> IO [(Revision, L.RawPointer)]
getRevisions Global{..} name = do
  root <- L.fetchRoot storage
  L.lookupSpine storage ("A" <> encodeUtf8 name) root >>= \case
    Just spine -> do
      result <- L.takeSpine storage 256 spine []
      traverse (\(tag, rp) -> case deserialise tag of
        Left e -> fail $ show e
        Right rev -> pure (rev, rp)) result
    Nothing -> return []

mainApp :: Global -> Application
mainApp global@Global{..} = unsafeStaticPolicy (addBase "static")
  $ \req sendResp -> case pathInfo req of
    ["auth-start"] -> authStart global sendResp
    ["auth-finish"] -> authFinish global req sendResp
    ["api", "revisions", page] -> do
      revs <- getRevisions global page
      sendResp $ responseLBS status200 [] $ J.encode $ map fst revs
    ["api", "articles", page, readMaybe . T.unpack -> Just i] -> do
      root <- L.fetchRoot storage
      L.lookupSpine storage ("A" <> encodeUtf8 page) root >>= \case
        Nothing -> sendResp notFound
        Just spine -> do
          spine' <- L.dropSpine storage (L.spineLength spine - i) spine
          result <- L.takeSpine storage 1 spine' []
          case result of
            (_, rp) : _ -> do
              bs <- L.fetchPayload storage rp
              sendResp $ responseLBS status200 [] $ J.encode $ T.decodeUtf8 bs
            _ -> sendResp $ responseLBS status404 [] "Not found"
    ["api", "file"] -> do
      body <- lazyRequestBody req
      case lookup "Content-Type" (Wai.requestHeaders req) of
        Nothing -> sendResp $ badRequest "Content-Type not found"
        Just typ -> case decodeImage $ BL.toStrict body of
          Left err -> sendResp $ responseBuilder status400 []
            $ getUtf8Builder $ fromString err
          Right dimg -> case makeThumbnail dimg of
            Nothing -> sendResp $ badRequest "Unsupported format"
            Just timg -> do
              L.commit storage $ do
                L.insert "images" (typ, body)
                L.insert "thumbnails" $ encodePng timg
              sendResp $ responseLBS status200 [] "Success"
    ["api", "file", readMaybe . T.unpack -> Just num] -> do
      root <- L.fetchRoot storage
      L.lookupSpine storage "images" root >>= \case
        Nothing -> sendResp notFound
        Just spine -> do
          spine' <- L.dropSpine storage (L.spineLength spine - num) spine
          result <- L.takeSpine storage 1 spine' []
          case result of
            [(_, rp)] -> do
              (typ, body) <- decodeCurrent <$> L.fetchPayload storage rp
              sendResp $ responseLBS status200 [("Content-Type", typ)] body
            _ -> sendResp notFound
    ["api", "thumbnails", readMaybe . T.unpack -> Just offset] -> do
      root <- L.fetchRoot storage
      L.lookupSpine storage "thumbnails" root >>= \case
        Nothing -> sendResp notFound
        Just spine -> do
          let len = L.spineLength spine
          spine' <- L.dropSpine storage offset spine
          result <- L.takeSpine storage 15 spine' []
          thumbnails <- forM (zip [len - offset, len - offset - 1..] result) $ \(i, (_, rp)) -> do
            body <- T.decodeUtf8 . Base64.encode <$> L.fetchPayload storage rp
            return $ J.object
              [ "id" J..= i
              , "src" J..= mappend "data:image/png;base64," body
              ]
          sendResp $ responseLBS status200 [] $ J.encode thumbnails
    _ -> sendResp $ responseFile status200 [] "index.html" Nothing
  where
    notFound = responseLBS status404 [] "Not found"
    badRequest = responseLBS status400 []
    Config{..} = config

makeThumbnail :: DynamicImage -> Maybe (Image PixelRGBA8)
makeThumbnail = \case
  ImageRGBA8 img -> Just $ resize img
  ImageRGB8 img -> Just $ promoteImage $ resize img
  ImageYCbCr8 img -> Just $ promoteImage (convertImage $ resize img :: Image PixelRGB8)
  _ -> Nothing
  where
    resize img
      | w <= h = scaleBilinear (floor $ 128 * w / h) 128 img
      | otherwise = scaleBilinear 128 (floor $ 128 * h / w) img
      where
        w = fromIntegral $ imageWidth img :: Double
        h = fromIntegral $ imageHeight img

main :: IO ()
main = evalContT $ do
  config@Config{..} <- ContT $ \k -> getArgs >>= \case
    [configPath] -> Yaml.decodeFileEither configPath >>= \case
      Right conf -> k conf
      Left err -> throwIO err
    _ -> fail "apus-exe config.yaml"
  hcManager <- newTlsManager
  logOptions' <- logOptionsHandle stderr True
  let logOptions = setLogUseTime True logOptions'
  logger <- ContT $ withLogFunc logOptions
  liftIO $ setLocaleEncoding utf8
  vEnvs <- newTVarIO HM.empty
  vUserInfo <- newTVarIO HM.empty
  vRecentChanges <- newTVarIO mempty
  storage <- ContT $ L.withLiszt dataPath
  liftIO $ runTLS
    (tlsSettings tlsCertificate tlsKey)
    (setPort port defaultSettings)
    $ websocketsOr defaultConnectionOptions
      (multiServer Global{..}) $ mainApp Global{..}
