{-# LANGUAGE OverloadedLabels, ScopedTypeVariables, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import RIO
import Control.Lens (lastOf, folded)
import Control.Monad.Trans.Cont
import Data.Aeson as J
import Data.Time.Clock
import Data.Winery
import qualified Data.Sequence as Seq
import qualified Database.Liszt as L
import GHC.Generics (Generic)
import GHC.IO.Encoding
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.Wai as Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.WebSockets
import Network.Wai.Middleware.Static hiding ((<|>))
import Network.WebSockets as WS
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import qualified RIO.HashMap as HM
import System.Environment

import qualified Api
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

data ApusException = Unchanged deriving (Show, Eq)
instance Exception ApusException

updateArticle :: Text -> Env -> Int -> Text -> STM (IO ())
updateArticle name Env{..} authorId theirs = do
  clientInfo <- readTVar vClientInfo
  authorName <- case IM.lookup authorId clientInfo of
    Just s -> return s
    Nothing -> fail "Unauthorised"
  m <- readTVar vClients

  modifyTVar (vRecentChanges global)
    $ \s -> ((name, T.takeWhile (/='\n') theirs, authorName) Seq.<|)
    $ if length s >= recentChangesCount (config global) then Seq.take (length s - 1) s else s

  return $ runRIO Env{..} $ do
    now <- liftIO getCurrentTime
    count <- L.count (storage global) (pageNameToKey name)
    liftIO $ L.commit (storage global) $ L.insertTagged (pageNameToKey name)
      (toEncodingWithSchema Revision
        { revId = count
        , revTime = now
        , revAuthor = authorName
        })
      theirs
    logInfo $ display authorName <> " updated " <> display name
    forM_ m $ \conn -> do
      revs <- liftIO $ fetchRevisions global name
      liftIO $ sendTextData conn $ J.encode $ Content (map fst revs) theirs
      `catch` \(e :: SomeException) -> logError $ display e

handleRequest :: Text -> Env -> WS.Connection -> Int -> ApusReq -> IO ()
handleRequest name Env{..} conn clientId = \case
  Submit doc -> join $ do
    atomically $ (>>broadcastChanges global) <$> updateArticle name Env{..} clientId doc
    <|> pure (pure ())
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
      revs <- fetchRevisions global name
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

mainApp :: Global -> Application
mainApp global@Global{..} = unsafeStaticPolicy (addBase "static")
  $ \req sendResp -> case pathInfo req of
    ["auth-start"] -> authStart global sendResp
    ["auth-finish"] -> authFinish global req sendResp
    "api" : xs -> sendResp =<< case xs of
      ["revisions", page] -> Api.getRevisions global page
      ["revisions", page, T.decimal -> Right (rev, _)] -> Api.getRevision global page rev
      ["file"] -> Api.postFile global req
      ["file", T.decimal -> Right (offset, _)] -> Api.getFile global offset
      ["thumbnails", T.decimal -> Right (offset, _)] -> Api.getThumbnails global offset
      _ -> return $ responseLBS status404 [] "Not found"
    _ -> sendResp $ responseFile status200 [] "index.html" Nothing
  where
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
  liftIO $ (case (tlsCertificate, tlsKey) of
    (Just cert, Just key) -> runTLS
      (tlsSettings cert key)
    _ -> runSettings)
    (setPort port defaultSettings)
    $ websocketsOr defaultConnectionOptions
      (multiServer Global{..}) $ mainApp Global{..}
