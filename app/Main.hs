{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, LambdaCase, OverloadedLabels, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import RIO

import Control.Monad.Trans.Cont
import Data.Aeson as J
import Data.Algorithm.Diff3
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
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import qualified Network.HTTP.Client as HC
import qualified RIO.HashMap as HM
import System.Directory
import System.Environment
import System.FilePath

import Auth.GitHub

data Global = Global
  { config :: Config
  , vUserInfo :: TVar (HM.HashMap AccessToken Text)
  , vEnvs :: TVar (HM.HashMap Text Env)
  , logger :: LogFunc
  , hcManager :: HC.Manager
  }

data Env = Env
  { vFreshClientId :: TVar Int
  , vClients :: TVar (IM.IntMap WS.Connection)
  , vCurrent :: TVar [T.Text]
  , vDraft :: TVar (IM.IntMap [T.Text])
  , vClientInfo :: TVar (IM.IntMap Text)
  , filePath :: FilePath
  , global :: Global
  }

data Config = Config
  { github :: GitHubInfo
  , dataDir :: FilePath
  , port :: Int
  } deriving Generic

instance FromJSON Config

instance HasLogFunc Global where
  logFuncL f e = (\l -> e { logger = l}) <$> f (logger e)

instance HasLogFunc Env where
  logFuncL = (\f e -> (\l -> e { global = l}) <$> f (global e)) . logFuncL

data ApusReq = Submit
  | Draft !Text
  | Heartbeat
  | Token !(Maybe AccessToken)
  deriving Generic
instance FromJSON ApusReq
instance ToJSON ApusReq

data ApusResp = Content !Text
  | HeartbeatAck
  | AuthAck !Text
  deriving Generic
instance FromJSON ApusResp
instance ToJSON ApusResp

hunkToText :: Hunk T.Text -> [T.Text]
hunkToText (LeftChange xs) = xs
hunkToText (RightChange xs) = xs
hunkToText (Unchanged xs) = xs
hunkToText (Conflict xs ys zs) = concat
  [ pure $ T.replicate 8 "<" <> " Their change"
  , xs
  , pure $ T.replicate 8 "|"
  , ys
  , pure $ T.replicate 8 "="
  , zs
  , pure $ T.replicate 8 ">" <> " Your change"
  ]

updateArticle :: Env -> Int -> [T.Text] -> STM (IO ())
updateArticle Env{..} authorId theirs = do
  clientInfo <- readTVar vClientInfo
  unless (IM.member authorId clientInfo)
    $ fail "Unauthorised"
  orig <- readTVar vCurrent
  writeTVar vCurrent theirs
  m <- readTVar vClients
  drafts <- readTVar vDraft
  return $ runRIO Env{..} $ do
    liftIO (T.writeFile filePath (T.unlines theirs))
      `catch` \(e :: SomeException) -> logError $ display e
    forM_ (IM.toList m) $ \(i, conn) -> do
      liftIO $ sendTextData conn $ J.encode $ Content $ T.unlines $ case IM.lookup i drafts of
        Just ours | i /= authorId -> concatMap hunkToText $ diff3 theirs orig ours
        _ -> theirs
      `catch` \(e :: SomeException) -> logError $ display e

handleRequest :: Env -> WS.Connection -> Int -> ApusReq -> IO ()
handleRequest Env{..} conn clientId = \case
  Submit -> join $ liftIO $ atomically $ do
    IM.lookup clientId <$> readTVar vDraft >>= \case
      Nothing -> return $ return ()
      Just doc -> updateArticle Env{..} clientId doc
  Draft txt -> atomically
    $ modifyTVar vDraft $ IM.insert clientId $! T.lines txt
  Heartbeat -> sendTextData conn $ J.encode HeartbeatAck
  Token (Just tok) -> join $ atomically $ do
    users <- readTVar vUserInfo
    case HM.lookup tok users of
      Just name -> do
        modifyTVar vClientInfo $ IM.insert clientId name
        return $ sendTextData conn $ J.encode $ AuthAck name
      Nothing -> return $ pure ()
  Token Nothing -> atomically $ modifyTVar vClientInfo $ IM.delete clientId
  where
    Global{..} = global

serverApp :: Env -> WS.ServerApp
serverApp Env{..} pending = do
  let Global{..} = global
  conn <- acceptRequest pending
  forkPingThread conn 10
  join $ atomically $ do
    i <- readTVar vFreshClientId
    writeTVar vFreshClientId $! i + 1
    modifyTVar vClients $ IM.insert i conn
    initialContent <- readTVar vCurrent
    return $ do
      liftIO $ sendTextData conn $ J.encode $ Content $ T.unlines initialContent
      forever $ do
        msg <- WS.receiveData conn
        case decode msg of
          Nothing -> runRIO Env{..} $ logError $ "Invalid message: " <> displayShow msg
          Just req -> handleRequest Env{..} conn i req

      `finally` atomically (modifyTVar vClients $ IM.delete i)
      `catch` \(e :: SomeException) -> runRIO Env{..} $ logError $ display e

multiServer :: Global -> WS.ServerApp
multiServer global@Global{..} pending = do
  envs <- atomically $ readTVar vEnvs
  env <- case HM.lookup name envs of
    Just env -> return env
    Nothing -> do
      vFreshClientId <- newTVarIO 0
      vClients <- newTVarIO IM.empty
      let filePath = dataDir config </> T.unpack name
      exist <- doesFileExist filePath
      vCurrent <- if exist
        then T.lines <$> T.readFile filePath >>= newTVarIO
        else do
          T.writeFile filePath ""
          newTVarIO []
      vDraft <- newTVarIO IM.empty
      vClientInfo <- newTVarIO IM.empty
      atomically $ modifyTVar vEnvs $ HM.insert name Env{..}
      return Env{..}
  serverApp env pending
  where
    name = sanitise $ T.decodeUtf8 $ B.drop 1 $ requestPath
      $ pendingRequest pending

sanitise :: T.Text -> T.Text
sanitise "" = "-"
sanitise t = T.map f t where
  f c
    | elem c ("/\\?%*:|'\"<>. " :: String) = '-'
    | otherwise = c

authStart :: Global -> (Wai.Response -> IO a) -> IO a
authStart Global{..} sendResp = sendResp $ responseLBS status200 [] $ BL.fromStrict $ mconcat
  [ "<html><head>"
  , "</head><body>"
  , "<script>"
  , "window.location.assign("
  , B.pack $ show $ oauthURL (github config) ""
  , ");"
  , "</script>"
  , "</body></html>"]

authFinish :: Global -> Wai.Request -> (Wai.Response -> IO a) -> IO a
authFinish Global{..} req sendResp = do
  let Just (Just code) = lookup "code" $ queryString req
  token@(AccessToken tokenStr) <- getAccessToken hcManager (github config) code
  name <- getUserName hcManager token
  atomically $ modifyTVar vUserInfo $ HM.insert token name
  sendResp $ responseLBS status200 [] $ BL.fromStrict $ mconcat
    [ "<html><head>"
    , "<script src=\"https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js\"></script>"
    , "</head><body>"
    , "<script>"
    , "Cookies.set('GitHubToken',"
    , B.pack $ show tokenStr
    , ");"
    , "</script>"
    , "Authorization finished. You may close this window"
    , "</body></html>"
    ]

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
  liftIO $ runEnv port
    $ websocketsOr defaultConnectionOptions
      (multiServer Global{..})
    $ unsafeStaticPolicy (addBase "static")
      $ \req sendResp -> case pathInfo req of
        ["auth-start"] -> authStart Global{..} sendResp
        ["auth-finish"] -> authFinish Global{..} req sendResp
        _ -> sendResp $ responseFile status200 [] "index.html" Nothing
