{-# LANGUAGE RecordWildCards #-}
module Sequence where

import RIO
import Control.Monad
import Control.Monad.STM (retry)
import Data.Binary
import Data.Binary.Get
import qualified Data.IntMap as IM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.FilePath
import System.Directory
import System.IO (SeekMode(..))

data Sequence = Sequence
  { hOffsets :: Handle
  , hPayload :: Handle
  , vOffsets :: TVar (IM.IntMap Int)
  }

withSequence :: FilePath -> (Sequence -> IO r) -> IO r
withSequence prefix k = do
  createDirectoryIfMissing True prefix
  initialOffsets <- withBinaryFile (prefix </> "offsets") ReadWriteMode B.hGetContents
  withBinaryFile (prefix </> "offsets") AppendMode $ \hOffsets ->
    withBinaryFile (prefix </> "payloads") ReadWriteMode $ \hPayload -> do
      vOffsets <- newTVarIO $! IM.fromList $ zip [0..]
        $ runGet (replicateM (B.length initialOffsets `div` 8) get)
        $ BL.fromStrict initialOffsets
      k Sequence{..}

write :: Sequence -> B.ByteString -> IO Int
write Sequence{..} bs = join $ atomically $ do
  indices <- readTVar vOffsets
  let (k', ofs) = case IM.maxViewWithKey indices of
        Nothing -> (0, B.length bs)
        Just ((k, v), _) -> (k + 1, B.length bs + v)
  writeTVar vOffsets $ IM.insert k' ofs indices
  return $ do
    hSeek hPayload SeekFromEnd 0
    B.hPutStr hPayload bs
    B.hPutStr hOffsets $ BL.toStrict $ encode ofs
    hFlush hPayload
    hFlush hOffsets
    return k'

fetch :: Sequence -> Int -> STM (IO B.ByteString)
fetch Sequence{..} i = do
  indices <- readTVar vOffsets
  (wing, ofs'', _) <- return $ IM.splitLookup i indices
  ofs' <- maybe retry pure ofs''
  let ofs = case IM.maxView wing of
        Nothing -> 0
        Just (v, _) -> v
  return $ do
    hSeek hPayload AbsoluteSeek (fromIntegral ofs)
    B.hGet hPayload (ofs' - ofs)
