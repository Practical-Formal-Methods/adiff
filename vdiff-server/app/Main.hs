{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where


import           System.IO
import           Web.Scotty.Trans


import           VDiff.Arguments
import           VDiff.Data
import           VDiff.Persistence
import           VDiff.Query             as Q

import           VDiff.Server.Controller
import           VDiff.Server.Prelude

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
  scottyT (port sp) (runRIO env) $ endpoints





