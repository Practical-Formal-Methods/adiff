{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module VDiff.Server.Controller where

import           VDiff.Server.Prelude
import           VDiff.Server.Widgets

import           Data.FileEmbed
import qualified Data.Text.Lazy                        as LT
import           Database.Beam
import           Database.Beam.Sqlite
import           Network.Wai.Middleware.StaticEmbedded
import           VDiff.Data
import           VDiff.Persistence
import qualified VDiff.Query                           as Q
import qualified VDiff.Query2                          as Q2


endpoints :: (HasDatabase env) => ScottyT SrvError (RIO env) ()
endpoints = do
  -- install static middleware
  middleware (static $(embedDir "static"))
  get "/" getIndex
  get "/program/:hash" getProgram
  get "/findings/" getFindings




getIndex :: (HasDatabase env) => RioActionM env ()
getIndex = do
  statistics <- lift $ Q2.stats
  let queriesSnippet = $(shamletFile "templates/queries.hamlet")
  defaultLayout "VDiff " $(shamletFile "templates/index.hamlet")

-- | shows all runs on one instrumented file
getProgram :: (HasDatabase env) => RioActionM env ()
getProgram = do
  hash <- param "hash"
  (runs :: [VerifierRun]) <- lift $ runBeam $ runSelectReturningList $ select $ Q2.runsByHash hash
  program <- mkProgramWidget hash
  defaultLayout ("program: " <> hash) $(shamletFile "templates/program.hamlet")


-- getQueries :: RioActionM env ()
-- getQueries = do
--   defaultLayout "Queries" $(shamletFile "templates/queries.hamlet")

getFindings :: (HasDatabase env) => RioActionM env ()
getFindings = do
  (qs :: String) <- param "q"
  liftIO $ putStrLn $ "query was " ++ qs
  (q :: Q2.Query) <- param "q"
  findings <- lift $ Q2.executeQuery q
  let pg = "PG"
  defaultLayout "Findings" $(shamletFile "templates/findings.hamlet")

instance Parsable Q2.Query where
    parseParam = mapLeft LT.fromStrict . Q2.parseQuery . LT.toStrict
