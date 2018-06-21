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
import           VDiff.Query                           as Q
import           Web.Scotty.Trans


data ServerParameters = ServerParameters
  { port       :: Int
  }



infos = (progDesc "viewer for vdiff")

main :: IO ()
main = error "useless until merge"





