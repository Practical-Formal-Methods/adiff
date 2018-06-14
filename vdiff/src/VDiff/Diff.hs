{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Main entry point. Implements all the commands that are called from Main
module VDiff.Diff where

import           VDiff.Prelude

import           Data.List             (sortBy)
import           Data.Ord              (comparing)
import qualified Data.Text.IO          as T
import           Language.C
import           Text.PrettyPrint      (render)

import           System.Directory
import           System.Exit
import           System.IO

import           VDiff.Instrumentation
import           VDiff.Strategy
import           VDiff.Verifier



cmdDiff :: HasMainEnv a => DiffParameters -> RIO a ()
cmdDiff params = do
  logInfo "starting diff"
  mAst <- openCFile (params ^. inputFile)
  case mAst of
    Nothing -> liftIO exitFailure
    Just ast -> do
      let ast' = preprocess ast
      stratEnv <- mkStrategyEnv ast' params
      runRIO stratEnv $ executeStrategy $ params ^. strategy


-- | parses the file, runs the semantic analysis (type checking), and pretty-prints the resulting semantic AST.
-- Use this to test the modified language-c-extensible library.
cmdParseTest :: HasLogFunc env => FilePath -> RIO env ()
cmdParseTest fn = openCFile fn >>= \case
  Nothing -> liftIO exitFailure
  Just ast -> liftIO $ putStrLn $ render $ pretty ast


cmdMarkReads :: HasLogFunc env => SearchMode -> FilePath -> RIO env ()
cmdMarkReads mode fn = do
  logDebug $ "mode is " <> display (tshow mode)
  (Just ast) <- openCFile fn
  let ast' = markAllReads mode ast
  liftIO . putStrLn . render . pretty $ ast'

cmdVersions :: RIO a ()
cmdVersions = liftIO $ forM_ (sortBy (comparing (^. name)) allVerifiers) $ \verifier -> do
    T.putStr $ verifier ^. name
    putStr ": "
    sv <- try (verifier ^. version) >>= \case
      Left (_ :: IOException) -> return "unknown (error)"
      Right Nothing -> return "unknown"
      Right (Just v) -> return v
    putStrLn sv

cmdRunVerifiers :: (HasLogFunc env) => DiffParameters -> RIO env ()
cmdRunVerifiers dp = do
  fn' <- liftIO $ makeAbsolute (dp ^. inputFile)
  lg <- view logFuncL
  let verifierEnv = VerifierEnv lg (dp ^. timelimit)
  forM_ (dp ^. verifiers) $ \v -> do
    liftIO $ print (v ^. name)
    res <- runRIO verifierEnv $ execute v fn'
    liftIO $ print res
