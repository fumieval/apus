{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Auth.GitHub where
import RIO
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.URI

data GitHubInfo = GitHubInfo
  { githubClientId :: ByteString
  , githubClientSecret :: ByteString
  } deriving Generic
instance FromJSON GitHubInfo where
  parseJSON = withObject "Object" $ \obj -> do
    githubClientId <- fmap T.encodeUtf8 $ obj .: "clientId"
    githubClientSecret <- fmap T.encodeUtf8 $ obj .: "clientSecret"
    return GitHubInfo{..}

newtype AccessToken = AccessToken ByteString deriving (Show, Eq, Hashable)

instance FromJSON AccessToken where
  parseJSON v = AccessToken . encodeUtf8 <$> parseJSON v

instance ToJSON AccessToken where
  toJSON (AccessToken t) = either (const Null) toJSON $ decodeUtf8' t

oauthURL :: GitHubInfo -> ByteString -> ByteString
oauthURL GitHubInfo{..} state = mconcat
  [ "https://github.com/login/oauth/authorize"
  , renderSimpleQuery True
    [ ("client_id", githubClientId)
    , ("state", state)
    ]
  ]

getAccessToken :: HC.Manager -> GitHubInfo -> B.ByteString -> IO AccessToken
getAccessToken man GitHubInfo{..} code = do
  initialRequest <- HC.parseRequest "https://github.com/login/oauth/access_token"
  resp <- HC.httpLbs (HC.urlEncodedBody
    [ ("client_id", githubClientId)
    , ("client_secret", githubClientSecret)
    , ("code", code)]
    initialRequest
      { HC.requestHeaders =
        [ ("Accept", "application/json")
        ]
      }) man
  either (fail . ("getAccessToken: "++)) return $ do
    val <- eitherDecode' $ HC.responseBody resp
    parseEither (.: "access_token") val

getUserName :: HC.Manager -> AccessToken -> IO T.Text
getUserName man (AccessToken token) = do
  initialRequest <- HC.parseRequest "https://api.github.com/user"
  resp <- HC.httpLbs initialRequest
    { HC.requestHeaders =
      [ ("Accept", "application/json")
      , ("Authorization", "token " <> token)
      , ("User-Agent", "https://github.com/fumieval/apus")
      ]
    } man
  either (fail . ("getUserName: "++)) return $ do
    val <- eitherDecode' $ HC.responseBody resp
    parseEither (.: "login") val
