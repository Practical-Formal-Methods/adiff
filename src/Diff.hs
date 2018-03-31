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
import           System.IO        (putStr, putStrLn)
import           Text.PrettyPrint (render)

import           Instrumentation
import           Persistence
import           Types
import           Verifier

import           RandomStrategy


-- | This is a RIO version of persist
persist' :: HasDatabase env => Persistent a => a -> RIO env ()
persist' x = do
  conn <- view databaseL
  liftIO $ persist conn x


cmdDiff :: HasMainEnv a => DiffParameters -> RIO a ()
cmdDiff params = do
  logInfo "starting diff"
  ast <- maskAsserts <$> openCFile (params ^. program)
  stratEnv <- mkStrategyEnv ast params
  case params ^. strategy of
    NaiveRandom -> do
      logInfo "using 'random' strategy"
      runRIO stratEnv randomStrategy



    _ -> error "strategy not implemented"
  undefined


-- | parses the file, runs the semantic analysis (type checking), and pretty-prints the resulting semantic AST.
-- Use this to test the modified language-c-extensible library.
cmdParseTest :: HasLogFunc env => FilePath -> RIO env ()
cmdParseTest fn = openCFile fn >>= liftIO . putStrLn . render . pretty


cmdMarkReads :: HasLogFunc env => FilePath -> RIO env ()
cmdMarkReads fn = liftIO . putStrLn . render . pretty . markAllReads =<< openCFile fn

cmdVersions :: RIO a ()
cmdVersions = liftIO $ forM_ (sortBy (comparing verifierName) allVerifiers) $ \verifier -> do
    putStr $ verifierName verifier
    putStr ": "
    sv <- try (version verifier) >>= \case
      Left (_ :: IOException) -> return "unknown (error)"
      Right Nothing -> return "unknown"
      Right (Just v) -> return v
    putStrLn sv
