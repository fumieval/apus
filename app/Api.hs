{-# LANGUAGE FlexibleContexts #-}
module Api where

import RIO
import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Extra
import Control.Monad.Trans.Cont
import Data.Aeson as J
import qualified Data.ByteString.Base64 as Base64
import Data.Time.Clock
import Data.Winery
import qualified Database.Liszt as L
import Network.HTTP.Types
import Network.Wai as Wai
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified RIO.HashMap as HM

import Auth.GitHub
import Types
import Util

getThumbnails :: Global -> Int -> IO Response
getThumbnails Global{..} offset = do
  result <- L.fetchRange storage "thumbnails" (-offset - 16) (-offset - 1)
  thumbnails <- forM result $ \(idx, _, rp) -> do
    body <- T.decodeUtf8 . Base64.encode <$> L.fetchPayload storage rp
    return $ J.object
      [ "id" J..= idx
      , "src" J..= mappend "data:image/png;base64," body
      ]
  return $ responseLBS status200 [] $ J.encode thumbnails

getFile :: Global -> Int -> IO Response
getFile Global{..} offset = do
  L.fetchRange storage "images" offset offset >>= \case
    [(_, _, rp)] -> do
      Right (ImageInfo _ _ typ body) <- deserialise <$> L.fetchPayload storage rp
      return $ responseLBS status200 [("Content-Type", typ)] $ BL.fromStrict body
    _ -> return notFound

postFile :: Global -> Request -> IO Response
postFile Global{..} req = evalContT $ do
  body_ <- liftIO $ lazyRequestBody req
  token <- lookup "Authorization" (Wai.requestHeaders req)
    ?? pure (badRequest "Missing token")
  users <- atomically $ readTVar vUserInfo
  author <- HM.lookup (AccessToken token) users ?? pure (badRequest "Invalid token")

  typ <- lookup "Content-Type" (Wai.requestHeaders req)
    ?? pure (badRequest "Content-Type not found")

  let body = BL.toStrict body_
  dimg <- decodeImage body ??? \err -> pure
    $ responseBuilder status400 [] $ getUtf8Builder $ fromString err

  timg <- makeThumbnail dimg ?? pure (badRequest "Unsupported format")

  now <- liftIO getCurrentTime

  L.commit storage $ do
    L.insert "images" $ serialise $ ImageInfo now author typ body
    L.insert "thumbnails" $ encodePng timg

  return $ responseLBS status200 [] "Success"

getRevisions :: Global -> Text -> IO Response
getRevisions global page = do
  revs <- fetchRevisions global page
  pure $ responseLBS status200 [] $ J.encode $ map fst revs

getRevision :: Global -> Text -> Int -> IO Response
getRevision Global{..} page rev = do
  L.fetchRange storage (pageNameToKey page) rev rev >>= \case
    [(_, _, rp)] -> do
      bs <- L.fetchPayload storage rp
      pure $ responseLBS status200 [] $ J.encode $ T.decodeUtf8 bs
    _ -> return notFound

notFound :: Response
notFound = responseLBS status404 [] "Not found"

badRequest :: Text -> Response
badRequest = responseLBS status400 [] . BL.fromStrict . encodeUtf8

makeThumbnail :: DynamicImage -> Maybe (Image PixelRGBA8)
makeThumbnail = \case
  ImageRGBA8 img -> Just $ resize img
  ImageRGB8 img -> Just $ promoteImage $ resize img
  ImageYCbCr8 img -> Just $ promoteImage (convertImage $ resize img :: Image PixelRGB8)
  _ -> Nothing
  where
    resize img
      | w <= h = scaleBilinear (floor $ 128 * w / h) 128 img
      | otherwise = scaleBilinear 128 (floor $ 128 * h / w) img
      where
        w = fromIntegral $ imageWidth img :: Double
        h = fromIntegral $ imageHeight img
