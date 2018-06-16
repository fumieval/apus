module Types where

import RIO

import Data.Aeson as J
import Data.Algorithm.Diff
import qualified Data.Sequence as Seq
import Data.Text.Encoding as T
import GHC.Generics (Generic)
import Network.WebSockets as WS
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import qualified RIO.HashMap as HM
import Data.Time.Clock

import Auth.GitHub
import Sequence

data Config = Config
  { github :: GitHubInfo
  , dataDir :: FilePath
  , port :: Int
  , recentChangesCount :: Int
  , searchLimit :: Int
  , tlsCertificate :: FilePath
  , tlsKey :: FilePath
  } deriving Generic

instance FromJSON Config

data Global = Global
  { config :: Config
  , vUserInfo :: TVar (HM.HashMap AccessToken Text)
  , vEnvs :: TVar (HM.HashMap Text Env)
  , vRecentChanges :: TVar (Seq.Seq (Text, Text, Text))
  , logger :: LogFunc
  , hcManager :: HC.Manager
  , vRevisions :: TVar (HM.HashMap Text [Revision])
  , storage :: Sequence
  }

instance HasLogFunc Global where
  logFuncL f e = (\l -> e { logger = l}) <$> f (logger e)

data Env = Env
  { vFreshClientId :: TVar Int
  , vClients :: TVar (IM.IntMap WS.Connection)
  , vCurrent :: TVar T.Text
  , vClientInfo :: TVar (IM.IntMap Text)
  , global :: Global
  }

data Revision = Revision
  { revId :: !Int
  , revAuthor :: !Text
  , revTime :: !UTCTime
  } deriving (Show, Generic)
instance FromJSON Revision
instance ToJSON Revision

instance HasLogFunc Env where
  logFuncL = (\f e -> (\l -> e { global = l}) <$> f (global e)) . logFuncL

renderDiff :: Int -> Diff Text -> [Text]
renderDiff i d = map (T.pack (show i) <>) $ case d of
  First a -> [" - " <> a]
  Second a -> [" + " <> a]
  _ -> []

createEnv :: Global -> Text -> IO Env
createEnv global@Global{..} name = do
  vFreshClientId <- newTVarIO 0
  vClients <- newTVarIO IM.empty
  revisions <- atomically $ readTVar vRevisions
  vCurrent <- case HM.lookup name revisions of
    Just (Revision i _ _ : _) -> join (atomically $ fetch storage i <|> pure (pure "")) >>= newTVarIO . decodeUtf8
    _ -> newTVarIO ""
  vClientInfo <- newTVarIO IM.empty
  atomically $ modifyTVar vEnvs $ HM.insert name Env{..}
  return Env{..}
