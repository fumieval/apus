module Auth where
import RIO

import Network.HTTP.Types
import Network.Wai as Wai
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified RIO.HashMap as HM

import Auth.GitHub
import Types

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
