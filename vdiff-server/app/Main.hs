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


data ServerParameters = ServerParameters
  { port       :: Int
  }



infos = (progDesc "viewer for vdiff")

main :: IO ()
main = error "useless until merge"





