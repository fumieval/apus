{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, LambdaCase, OverloadedLabels, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import RIO

import Control.Monad.Trans.Cont
import Data.Aeson as J
import Data.Algorithm.Diff
import Data.List (zipWith)
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
  , vCurrent :: TVar T.Text
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

data ApusReq = Submit !Text
  | Token !(Maybe AccessToken)
  deriving Generic
instance FromJSON ApusReq
instance ToJSON ApusReq

data ApusResp = Content !Text
  | AuthAck !Text
  deriving Generic
instance FromJSON ApusResp
instance ToJSON ApusResp

renderDiff :: Int -> Diff Text -> [Text]
renderDiff i d = map (T.pack (show i) <>) $ case d of
  First a -> [" - " <> a]
  Second a -> [" + " <> a]
  _ -> []

updateArticle :: Env -> Int -> T.Text -> STM (IO ())
updateArticle Env{..} authorId theirs = do
  clientInfo <- readTVar vClientInfo
  authorName <- case IM.lookup authorId clientInfo of
    Just name -> return name
    Nothing -> fail "Unauthorised"
  original <- readTVar vCurrent
  writeTVar vCurrent theirs
  m <- readTVar vClients
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

handleRequest :: Env -> WS.Connection -> Int -> ApusReq -> IO ()
handleRequest Env{..} conn clientId = \case
  Submit doc -> join $ atomically $ updateArticle Env{..} clientId doc
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
      liftIO $ sendTextData conn $ J.encode $ Content initialContent
      forever $ do
        msg <- WS.receiveData conn
        case decode msg of
          Nothing -> WS.sendClose conn ("Invalid message" :: Text)
          Just req -> handleRequest Env{..} conn i req

      `finally` atomically (do
        modifyTVar vClients $ IM.delete i
        modifyTVar vClientInfo $ IM.delete i)
      `catch` \(_ :: ConnectionException) -> return ()

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
        then T.readFile filePath >>= newTVarIO
        else newTVarIO ""
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
        ["api", "search", query] -> do
          xs <- listDirectory dataDir
          resp <- fmap concat $ forM xs $ \path -> do
            content <- T.readFile (dataDir </> path)
            let title = T.takeWhile (/='\n') content
            let results = take 1 $ T.breakOnAll query content
            return [(path, title, T.takeEnd 30 pre, T.take 30 $ T.drop (T.length query) post) | (pre, post) <- results]
          sendResp $ responseLBS status200 [] $ J.encode resp
        _ -> sendResp $ responseFile status200 [] "index.html" Nothing
