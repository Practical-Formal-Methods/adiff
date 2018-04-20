{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Main entry point. Implements all the commands that are called from Main
module VDiff.Diff where

import           RIO

import           Data.List             (sortBy)
import           Data.Ord              (comparing)
import           Language.C
import           Text.PrettyPrint      (render)

import           System.Directory
import           System.Exit
import           System.IO

import           VDiff.Instrumentation
import           VDiff.Strategy.Random
import           VDiff.Strategy.Smart
import           VDiff.Types
import           VDiff.Verifier



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

cmdRunVerifiers :: (HasLogFunc env) => [Verifier] -> FilePath -> RIO env ()
cmdRunVerifiers vs fn = do
  fn' <- liftIO $ makeAbsolute fn
  lg <- view logFuncL
  let tl = 15 * 1000 * 1000
      verifierEnv = VerifierEnv lg tl
  forM_ vs $ \v -> do
    liftIO $ print (verifierName v)
    res <- runRIO verifierEnv $ execute v fn'
    liftIO $ print res
