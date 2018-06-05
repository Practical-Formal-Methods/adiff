{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           VDiff.Prelude

import qualified Data.Text.Lazy                as LT
import qualified Database.SQLite.Simple        as SQL
import           System.IO
import           Text.Hamlet
import           Web.Scotty.Trans


import           VDiff.Arguments
import           VDiff.Persistence
import qualified VDiff.Query                   as Q

import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)


data ServerParameters = ServerParameters
  { port       :: Int
  }


parseServerParameters :: Parser ServerParameters
parseServerParameters = ServerParameters <$> port
  where port = option auto (long "port" <> value 8080)


--------------------------------------------------------------------------------
-- Some Scotty Setup
type SrvError = LT.Text
type RioScottyM env = ScottyT SrvError (RIO env)
type RioActionM env = ActionT SrvError (RIO env)

--------------------------------------------------------------------------------

renderHamlet :: (Monad m) => Html -> ActionT e m ()
renderHamlet x = raw $ renderHtml x

infos = (progDesc "viewer for vdiff")

main :: IO ()
main = runVDiffApp parseServerParameters infos $ \sp -> do
  env <- ask
  scottyT (port sp) (runRIO env) $ do
    get "/" getIndex
    get "/runs/:hash" getRuns
    get "/query/:aspect" getQuery


getIndex :: RioActionM env ()
getIndex = do
  st <- lift $ ask
  html "hi"

-- | shows all runs on one instrumented file
getRuns :: (HasDatabase env) => RioActionM env ()
getRuns = do
  hash <- param "hash"
  runs <- lift $ Q.allRunsByHash hash
  renderHamlet $ $(hamletFile "templates/runs.tpl") hash

getQuery :: RioActionM env ()
getQuery = undefined
