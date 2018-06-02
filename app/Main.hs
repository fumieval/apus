{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, LambdaCase, OverloadedLabels, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import RIO

import Control.Monad.Trans.Cont
import Data.Aeson as J
import Data.Algorithm.Diff
import Data.Drinkery
import qualified Data.Drinkery.Finite as D
import Data.List (zipWith)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import GHC.IO.Encoding
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.Wai as Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.Wai.Middleware.Static
import Network.WebSockets as WS
import qualified Data.ByteString.Char8 as B
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
import Page
import Types

data ApusReq = Submit !Text
  | Token !(Maybe AccessToken)
  deriving Generic
instance FromJSON ApusReq
instance ToJSON ApusReq

data ApusResp = Content !Text
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
  original <- readTVar vCurrent
  writeTVar vCurrent theirs
  m <- readTVar vClients

  modifyTVar (vRecentChanges global)
    $ \s -> (Seq.|> (name, T.takeWhile (/='\n') theirs, authorName))
    $ if length s >= recentChangesCount (config global) then Seq.drop 1 s else s

  return $ runRIO Env{..} $ do
    liftIO (T.writeFile filePath theirs)
      `catch` \(e :: SomeException) -> logError $ display e
    let diff = concat $ zipWith renderDiff [1..]
          $ getDiff (T.lines original) (T.lines theirs)
    liftIO $ T.appendFile "apus-history" $ T.unlines
      $ authorName
      : T.pack filePath
      : T.pack (show (length diff))
      : diff
    logInfo $ display authorName <> " updated " <> displayShow filePath
    forM_ m $ \conn -> do
      liftIO $ sendTextData conn $ J.encode $ Content theirs
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
    initialContent <- readTVar vCurrent
    return $ do
      liftIO $ sendTextData conn $ J.encode $ Content initialContent
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
    name = sanitise $ T.decodeUtf8 $ B.drop 1 $ requestPath
      $ pendingRequest pending

mainApp :: Global -> Application
mainApp global@Global{..} = unsafeStaticPolicy (addBase "static")
  $ \req sendResp -> case pathInfo req of
    ["auth-start"] -> authStart global sendResp
    ["auth-finish"] -> authFinish global req sendResp
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
  liftIO $ runEnv port
    $ websocketsOr defaultConnectionOptions
      (multiServer Global{..}) $ mainApp Global{..}
