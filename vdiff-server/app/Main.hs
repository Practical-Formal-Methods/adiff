{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where


import           Network.Wai.Middleware.StaticEmbedded
import           System.IO
import           VDiff.Arguments
import           VDiff.Data
import           VDiff.Persistence
import           Web.Scotty.Trans

import           VDiff.Server.Controller
import           VDiff.Server.Prelude

import qualified VDiff.Query2                          as Q2

data ServerParameters = ServerParameters
  { port         :: Int
  , forceRecount :: Bool
  }


parseServerParameters :: Parser ServerParameters
parseServerParameters = ServerParameters <$> port <*> switchForceRecount
  where
    port = option auto (long "port" <> value 8080)
    switchForceRecount = switch (long "force-recount" <> help "re-calculates the temporary table that contains all counts of sats/unsats")


infos = (progDesc "viewer for vdiff")

main :: IO ()
main = runVDiffApp parseServerParameters infos $ \sp -> do
  logInfo "starting vdiff-server"
  env <- ask
  updateTable <- do
    if forceRecount sp
      then return True
      else Q2.updateCountsTableNecessary
  when updateTable $ do
    Q2.updateCountsTableProgressive
  scottyT (port sp) (runRIO env) $ endpoints


