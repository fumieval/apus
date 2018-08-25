{-# LANGUAGE OverloadedLabels, ScopedTypeVariables, ViewPatterns #-}
module Main where

import RIO
import Control.Lens (lastOf, folded)
import Control.Monad.Trans.Cont
import Data.Aeson as J
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
    _ -> sendResp notFound
  where
    notFound = responseFile status200 [] "index.html" Nothing
    Config{..} = config

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
