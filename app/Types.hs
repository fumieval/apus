module Types where

import RIO

import Data.Aeson as J
import Data.Algorithm.Diff
import qualified Data.Sequence as Seq
import Database.Liszt (LisztHandle, fetchRange, Key, RawPointer)
import GHC.Generics (Generic)
import Network.WebSockets as WS
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import qualified RIO.HashMap as HM
import Data.Time.Clock
import Data.Winery

import Auth.GitHub

data Config = Config
  { github :: GitHubInfo
  , dataPath :: FilePath
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
  , storage :: LisztHandle
  }

instance HasLogFunc Global where
  logFuncL f e = (\l -> e { logger = l}) <$> f (logger e)

data Env = Env
  { vFreshClientId :: TVar Int
  , vClients :: TVar (IM.IntMap WS.Connection)
  , vClientInfo :: TVar (IM.IntMap Text)
  , global :: Global
  }

data Revision = Revision
  { revId :: !Int
  , revTime :: !UTCTime
  , revAuthor :: !Text
  } deriving (Show, Generic)
instance Serialise Revision
instance FromJSON Revision
instance ToJSON Revision

data ImageInfo = ImageInfo
  { imgTime :: !UTCTime
  , imgAuthor :: Text
  , imgType :: !ByteString
  , imgContent :: !ByteString
  } deriving Generic
instance Serialise ImageInfo

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
  vClientInfo <- newTVarIO IM.empty
  atomically $ modifyTVar vEnvs $ HM.insert name Env{..}
  return Env{..}

fetchRevisions :: Global -> Text -> IO [(Revision, RawPointer)]
fetchRevisions Global{..} name = do
  result <- fetchRange storage (pageNameToKey name) (-256) (-1)
  traverse (\(_, tag, rp) -> case deserialise tag of
    Left e -> fail $ show e
    Right rev -> pure (rev, rp)) result

pageNameToKey :: Text -> Key
pageNameToKey name = "A" <> encodeUtf8 name
