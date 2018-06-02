{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase, OverloadedLabels, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Page where

import RIO

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified RIO.HashMap as HM
import System.Directory
import System.FilePath

import Types

createEnv :: Global -> Text -> IO Env
createEnv global@Global{..} name = do
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
