{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Main entry point. Implements all the commands that are called from Main
module Diff where

import           RIO

import           Data.List        (sortBy)
import           Data.Ord         (comparing)
import           Language.C
import           Text.PrettyPrint (render)

import           System.Exit
import           System.IO

import           Instrumentation
import           Persistence
import           Types
import           Verifier

import           Strategy.Random
import           Strategy.Smart


-- | This is a RIO version of persist
persist' :: HasDatabase env => Persistent a => a -> RIO env ()
persist' x = do
  conn <- view databaseL
  liftIO $ persist conn x


cmdDiff :: HasMainEnv a => DiffParameters -> RIO a ()
cmdDiff params = do
  logInfo "starting diff"
  mAst <- openCFile (params ^. program)
  case mAst of
    Nothing -> liftIO exitFailure
    Just ast -> do
      let astMasked = maskAsserts ast
      stratEnv <- mkStrategyEnv astMasked params
      case params ^. strategy of
        RandomStrategy -> do
          logInfo "using 'random' strategy"
          runRIO stratEnv randomStrategy
        SmartStrategy -> do
          logInfo "using 'smart' strategy"
          runRIO stratEnv smartStrategy
        _ -> error "strategy not implemented"


-- | parses the file, runs the semantic analysis (type checking), and pretty-prints the resulting semantic AST.
-- Use this to test the modified language-c-extensible library.
cmdParseTest :: HasLogFunc env => FilePath -> RIO env ()
cmdParseTest fn = openCFile fn >>= liftIO . putStrLn . render . maybe "" pretty


cmdMarkReads :: HasLogFunc env => FilePath -> RIO env ()
cmdMarkReads fn = do
  (Just ast) <- openCFile fn
  let ast' = markAllReads ast
  liftIO . putStrLn . render . pretty $ ast'

cmdVersions :: RIO a ()
cmdVersions = liftIO $ forM_ (sortBy (comparing verifierName) allVerifiers) $ \verifier -> do
    putStr $ verifierName verifier
    putStr ": "
    sv <- try (version verifier) >>= \case
      Left (_ :: IOException) -> return "unknown (error)"
      Right Nothing -> return "unknown"
      Right (Just v) -> return v
    putStrLn sv
