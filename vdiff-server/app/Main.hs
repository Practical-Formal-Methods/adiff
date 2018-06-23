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

import qualified VDiff.Query2 as Q2

data ServerParameters = ServerParameters
  { port       :: Int
  }


parseServerParameters :: Parser ServerParameters
parseServerParameters = ServerParameters <$> port
  where port = option auto (long "port" <> value 8080)


infos = (progDesc "viewer for vdiff")

main :: IO ()
main = runVDiffApp parseServerParameters infos $ \sp -> do
  env <- ask
  whenM Q2.updateCountsTableNecessary $ do
    logInfo "update necessary"
    Q2.updateCountsTableProgressive
  scottyT (port sp) (runRIO env) $ endpoints


