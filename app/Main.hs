{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, LambdaCase, OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import RIO
import Web.Scotty as S
import Data.Aeson as J
import Data.Extensible
import Data.Extensible.GetOpt
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import Network.WebSockets as WS
import Network.Wai.Middleware.Static
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp
import Data.Algorithm.Diff3

data Env = Env
  { vFreshClientId :: TVar Int
  , vClients :: TVar (IM.IntMap WS.Connection)
  , vCurrent :: TVar [T.Text]
  , vDraft :: TVar (IM.IntMap [T.Text])
  , filePath :: FilePath
  }

data ApusReq = Submit
  | Draft !Text
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
  return $ do
    T.writeFile filePath (T.unlines theirs)
    forM_ (IM.toList m) $ \(i, conn) -> do
      sendTextData conn $ T.unlines $ case IM.lookup i drafts of
        Just ours | i /= authorId -> concatMap hunkToText $ diff3 theirs orig ours
        _ -> theirs

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
          Just Submit -> join $ atomically $ IM.lookup i <$> readTVar vDraft >>= \case
            Nothing -> return $ return ()
            Just doc -> updateArticle Env{..} i doc
          Just (Draft txt) -> atomically $ modifyTVar vDraft
            $ IM.insert i $! T.lines txt

      `finally` atomically (modifyTVar vClients $ IM.delete i)

main :: IO ()
main = withGetOpt "PATH" opts $ \opt [filePath] -> do
  vFreshClientId <- newTVarIO 0
  vClients <- newTVarIO IM.empty
  vCurrent <- T.lines <$> T.readFile filePath >>= newTVarIO
  vDraft <- newTVarIO IM.empty
  app <- scottyApp $ do
    middleware $ unsafeStaticPolicy $ addBase "static"
    get "/" $ file "index.html"
  runEnv (maybe 9960 id $ opt ^. #port >>= readMaybe)
    $ websocketsOr defaultConnectionOptions (serverApp Env{..}) app
  where
    opts = #port @= optLastArg "p" ["port"] "port" "PORT"
      <: nil
