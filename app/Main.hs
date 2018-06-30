{-# LANGUAGE OverloadedLabels, ScopedTypeVariables, ViewPatterns #-}
module Main where

import RIO
import Control.Monad.STM (retry, throwSTM)
import Control.Monad.Trans.Cont
import Data.Aeson as J
import Data.Drinkery
import qualified Data.Drinkery.Finite as D
import Data.Time.Clock
import qualified Data.Sequence as Seq
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
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import qualified RIO.HashMap as HM
import System.Directory
import System.Environment
import System.FilePath

import Auth.GitHub
import Auth
import Sequence
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
  current <- readTVar vCurrent
  when (current == theirs) retry
  writeTVar vCurrent theirs
  m <- readTVar vClients

  modifyTVar (vRecentChanges global)
    $ \s -> ((name, T.takeWhile (/='\n') theirs, authorName) Seq.<|)
    $ if length s >= recentChangesCount (config global) then Seq.take (length s - 1) s else s

  return $ runRIO Env{..} $ do
    newId <- liftIO $ write (storage global) $ T.encodeUtf8 theirs
    now <- liftIO getCurrentTime
    revs <- atomically $ do
      modifyTVar (vRevisions global)
        $ HM.insertWith (++) name [Revision newId authorName now]
      readTVar (vRevisions global)
    liftIO $ BL.writeFile (dataDir (config global) </> "revisions") $ J.encode revs
    logInfo $ display authorName <> " updated " <> display name
    forM_ m $ \conn -> do
      liftIO $ sendTextData conn $ J.encode $ Content (maybe [] id $ HM.lookup name revs) theirs
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
    initialContent <- readTVar vCurrent
    revMap <- readTVar vRevisions
    let revs = maybe [] id $ HM.lookup name revMap
    return $ do
      liftIO $ sendTextData conn $ J.encode $ Content revs initialContent
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
    ["api", "revisions", page] -> join $ atomically $ asum
      [ do
          revMap <- readTVar vRevisions
          revs <- maybe retry pure $ HM.lookup page revMap
          return $ sendResp $ responseLBS status200 [] $ J.encode revs
      , return $ sendResp $ responseLBS status404 [] "Not found"
      ]
    ["api", "articles", readMaybe . T.unpack -> Just i] -> join $ atomically $ asum
      [ do
          getPage <- fetch storage i
          return $ do
            content <- getPage
            sendResp $ responseLBS status200 [] $ J.encode (T.decodeUtf8 content)
      , return $ sendResp $ responseLBS status404 [] "Not found"
      ]
    ["api", "search", query] -> tapListT' (do
      path <- liftIO (listDirectory dataDir) >>= sample
      content <- liftIO $ T.readFile $ dataDir </> path

      let title = T.takeWhile (/='\n') content
      let results = T.breakOnAll query content
      sample [(path, title, T.takeEnd 30 pre, T.take 30 $ T.drop (T.length query) post) | (pre, post) <- results]
      ) +& D.take searchLimit $& do
        resp <- D.drinkUp
        liftIO $ sendResp $ responseLBS status200 [] $ J.encode resp
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
  vRevisions <- liftIO $ do
    exist <- doesFileExist (dataDir </> "revisions")
    if exist
      then do
        revs <- J.eitherDecode' <$> BL.readFile (dataDir </> "revisions")
        newTVarIO $ either (const HM.empty) id revs
      else do
        B.writeFile (dataDir </> "revisions") "{}"
        newTVarIO HM.empty
  storage <- ContT $ withSequence dataDir
  liftIO $ runEnv 9960
    -- (tlsSettings tlsCertificate tlsKey)
    -- (setPort port defaultSettings)
    $ websocketsOr defaultConnectionOptions
      (multiServer Global{..}) $ mainApp Global{..}
