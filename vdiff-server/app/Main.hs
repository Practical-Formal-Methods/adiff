{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Control.Concurrent.MSemN              as Sema
import           Network.Wai.Middleware.StaticEmbedded
import           System.IO
import           VDiff.Arguments
import           VDiff.Data
import           VDiff.Persistence
import           Web.Scotty.Trans

import           VDiff.Application
import           VDiff.Server.Controller
import           VDiff.Server.Prelude

import qualified VDiff.Query2                          as Q2

data ServerParameters = ServerParameters
  { port                   :: Int
  , forceRecount           :: Bool
  , maxConcurrentVerifiers :: Int
  }


parseServerParameters :: Parser ServerParameters
parseServerParameters = ServerParameters <$> port <*> switchForceRecount <*> maxConcurrent
  where
    port = option auto (short 'p' <> long "port" <> value 8080)
    switchForceRecount = switch (long "force-recount" <> help "re-calculates the temporary table that contains all counts of sats/unsats")
    maxConcurrent = option auto (short 'n' <> long "max-concurrent-verifiers" <> value 1)


infos = progDesc "runs a webserver to inspect vdiff results"

main :: IO ()
main = runVDiffApp parseServerParameters infos $ \sp -> do
  logInfo "starting vdiff-server"
  sema <- liftIO $ Sema.new (maxConcurrentVerifiers sp)
  env <- mkServerEnv sema

  -- clean up DB (this is important for the correctness of consensus)
  Q2.cleanUp

  -- update counts table
  updateCountsTable <-
    if forceRecount sp
    then return True
    else Q2.updateCountsTableNecessary

  -- update consensus
  updateConsensusTable <-
    if forceRecount sp
    then return True
    else Q2.updateConsensusTableNecessary

  when updateCountsTable $ do
    logInfo "updating counts table"
    Q2.updateCountsTableProgressive

  when updateConsensusTable $ do
    logInfo "updating consensus table"
    Q2.updateConsensus

  -- start server
  scottyT (port sp) (runRIO env) endpoints


mkServerEnv :: Sema.MSemN Int -> RIO MainEnv ServerEnv
mkServerEnv s = do
  menv <- ask
  tables <- newIORef Nothing
  return $ ServerEnv menv s tables
