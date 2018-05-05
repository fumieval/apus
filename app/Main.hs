{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, LambdaCase, OverloadedLabels, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import RIO
import Web.Scotty as S
import qualified Data.ByteString.Char8 as B
import Data.Aeson as J
import Data.Extensible
import Data.Extensible.GetOpt
import qualified RIO.HashMap as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Network.WebSockets as WS
import Network.Wai.Middleware.Static
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp
import Data.Algorithm.Diff3
import GHC.IO.Encoding
import System.FilePath
import System.Directory

data Env = Env
  { vFreshClientId :: TVar Int
  , vClients :: TVar (IM.IntMap WS.Connection)
  , vCurrent :: TVar [T.Text]
  , vDraft :: TVar (IM.IntMap [T.Text])
  , filePath :: FilePath
  , logger :: LogFunc
  }

instance HasLogFunc Env where
  logFuncL f e = (\l -> e { logger = l}) <$> f (logger e)

data ApusReq = Submit
  | Draft !Text
  | Heartbeat
  deriving Generic
instance FromJSON ApusReq
instance ToJSON ApusReq

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
  orig <- readTVar vCurrent
  writeTVar vCurrent theirs
  m <- readTVar vClients
  drafts <- readTVar vDraft
  return $ runRIO Env{..} $ do
    liftIO (T.writeFile filePath (T.unlines theirs))
      `catch` \(e :: SomeException) -> logError $ display e
    forM_ (IM.toList m) $ \(i, conn) -> do
      liftIO $ sendTextData conn $ T.unlines $ case IM.lookup i drafts of
        Just ours | i /= authorId -> concatMap hunkToText $ diff3 theirs orig ours
        _ -> theirs
      `catch` \(e :: SomeException) -> logError $ display e

serverApp :: Env -> WS.ServerApp
serverApp Env{..} pending = do
  conn <- acceptRequest pending
  join $ atomically $ do
    i <- readTVar vFreshClientId
    writeTVar vFreshClientId $! i + 1
    modifyTVar vClients $ IM.insert i conn
    return $ do
      initialContent <- atomically $ readTVar vCurrent
      sendTextData conn $ T.unlines initialContent
      forever $ do
        J.decode <$> WS.receiveData conn >>= \case
          Nothing -> fail "Invalid Message"
          Just Submit -> join $ liftIO $ atomically $ IM.lookup i <$> readTVar vDraft >>= \case
            Nothing -> return $ return ()
            Just doc -> updateArticle Env{..} i doc
          Just (Draft txt) -> atomically $ modifyTVar vDraft
            $ IM.insert i $! T.lines txt
          Just Heartbeat -> return ()

      `finally` atomically (modifyTVar vClients $ IM.delete i)
      `catch` \(e :: SomeException) -> runRIO Env{..} $ logError $ display e

multiServer :: LogFunc -> FilePath -> TVar (HM.HashMap T.Text Env) -> WS.ServerApp
multiServer logger dir vEnvs pending = do
  envs <- atomically $ readTVar vEnvs
  env <- case HM.lookup name envs of
    Just env -> return env
    Nothing -> do
      vFreshClientId <- newTVarIO 0
      vClients <- newTVarIO IM.empty
      let filePath = dir </> T.unpack name
      exist <- doesFileExist filePath
      vCurrent <- if exist
        then T.lines <$> T.readFile filePath >>= newTVarIO
        else do
          T.writeFile filePath ""
          newTVarIO []
      vDraft <- newTVarIO IM.empty
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

main :: IO ()
main = withGetOpt "" opts $ \opt _ -> do
  logOptions' <- logOptionsHandle stderr True
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \logger -> do
    let fileDir = maybe "data" id $ opt ^. #file
    setLocaleEncoding utf8
    vEnvs <- newTVarIO HM.empty
    app <- scottyApp $ do
      middleware $ unsafeStaticPolicy $ addBase "static"
      get "/:page" $ file "index.html"
    runEnv (maybe 9960 id $ opt ^. #port >>= readMaybe)
      $ websocketsOr defaultConnectionOptions (multiServer logger fileDir vEnvs) app
    where
      opts = #port @= optLastArg "p" ["port"] "port" "PORT"
        <: #file @= optLastArg "d" ["dir"] "Content directory" "PATH"
        <: nil
