{-# LANGUAGE LambdaCase        #-}
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
import           VDiff.Data
import           VDiff.Persistence
import           VDiff.Query                   as Q

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

defaultTemplate :: String -> Html -> Html
defaultTemplate title content = $(shamletFile "templates/template.hamlet")

defaultLayout title content = raw $ renderHtml $ defaultTemplate title content

infos = (progDesc "viewer for vdiff")

main :: IO ()
main = runVDiffApp parseServerParameters infos $ \sp -> do
  env <- ask
  scottyT (port sp) (runRIO env) $ do
    get "/" getIndex
    get "/program/:hash" getProgram
    get "/query/" getQueries
    get "/query/:query" getQuery


getIndex :: (HasDatabase env) => RioActionM env ()
getIndex = do
  statistics <- lift $ Q.stats
  let queriesSnippet = $(shamletFile "templates/queries.hamlet")
  defaultLayout "VDiff " $(shamletFile "templates/index.hamlet")


mkProgramLink :: String -> Html
mkProgramLink hsh =
  let trunc = take 5 hsh
  in [shamlet| <a href="/program/#{hsh}">#{trunc}|]


-- | creates a program "widget"
mkProgramWidget :: (HasDatabase env) => String -> RioActionM env Html
mkProgramWidget hash = do
  (Just program) <- lift $ Q.programByHash hash
  return $(shamletFile "templates/widgets/source.hamlet")


-- | shows all runs on one instrumented file
getProgram :: (HasDatabase env) => RioActionM env ()
getProgram = do
  hash <- param "hash"
  runs <- lift $ Q.allRunsByHash hash
  program <- mkProgramWidget hash
  defaultLayout ("program: " ++ hash) $(shamletFile "templates/program.hamlet")


getQueries :: RioActionM env ()
getQueries = do
  defaultLayout "Queries" $(shamletFile "templates/queries.hamlet")

getQuery :: (HasDatabase env) => RioActionM env ()
getQuery = do
  (Just q) <- Q.queryFromName <$> param "query"
  findings <- lift $ Q.executeQuery q
  defaultLayout "Findings" $(shamletFile "templates/findings.hamlet")

