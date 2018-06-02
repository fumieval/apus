module Types where

import RIO

import Data.Aeson as J
import Data.Algorithm.Diff
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Network.WebSockets as WS
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import qualified RIO.HashMap as HM

import Auth.GitHub

data Config = Config
  { github :: GitHubInfo
  , dataDir :: FilePath
  , port :: Int
  , recentChangesCount :: Int
  , searchLimit :: Int
  } deriving Generic

instance FromJSON Config

data Global = Global
  { config :: Config
  , vUserInfo :: TVar (HM.HashMap AccessToken Text)
  , vEnvs :: TVar (HM.HashMap Text Env)
  , vRecentChanges :: TVar (Seq.Seq (Text, Text, Text))
  , logger :: LogFunc
  , hcManager :: HC.Manager
  }

instance HasLogFunc Global where
  logFuncL f e = (\l -> e { logger = l}) <$> f (logger e)

data Env = Env
  { vFreshClientId :: TVar Int
  , vClients :: TVar (IM.IntMap WS.Connection)
  , vCurrent :: TVar T.Text
  , vClientInfo :: TVar (IM.IntMap Text)
  , filePath :: FilePath
  , global :: Global
  }

instance HasLogFunc Env where
  logFuncL = (\f e -> (\l -> e { global = l}) <$> f (global e)) . logFuncL

sanitise :: T.Text -> T.Text
sanitise "" = "-"
sanitise t = T.map f t where
  f c
    | elem c ("/\\?%*:|'\"<>. " :: String) = '-'
    | otherwise = c

renderDiff :: Int -> Diff Text -> [Text]
renderDiff i d = map (T.pack (show i) <>) $ case d of
  First a -> [" - " <> a]
  Second a -> [" + " <> a]
  _ -> []
