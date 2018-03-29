{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Diff where

import           RIO

import           Control.Lens.At
import           Control.Lens.Operators
import           Data.List                        (sortBy)
import           Data.Ord                         (comparing)
import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.TravMonad
import           Language.C.Data.Lens
import           System.Exit
import           System.IO                        (hPutStr, putStr, putStrLn)
import           Text.PrettyPrint                 (render)

import           Data
import           Instrumentation
import           Persistence
import           Types
import           Verifier

import           RandomStrategy


-- short-hand for open, parse and type annotate
openCFile :: HasLogFunc env => FilePath -> RIO env (CTranslationUnit SemPhase)
openCFile fn = do
  x <- liftIO $ parseCFilePre fn
  case x of
    Left parseError -> do
      logError $ "parse error: " <> displayShow parseError
      liftIO exitFailure
    Right tu -> case runTrav_ (analyseAST tu) of
        Left typeError -> do
          logError $ "type error: " <> displayShow typeError
          liftIO exitFailure
        Right (tu', warnings) -> do
          unless (null warnings) $ logWarn $ "warnings: " <> displayShow warnings
          return tu'


-- | This is a RIO version of persist
persist' :: HasDatabase env => Persistent a => a -> RIO env ()
persist' x = do
  conn <- view databaseL
  liftIO $ persist conn x


cmdDiff :: HasMainEnv a => DiffParameters -> RIO a ()
cmdDiff DiffParameters{..} = do
  logInfo "starting diff"
  ast <- maskAsserts <$> openCFile program
  let (Just stmt) = ast ^? ix "main" . functionDefinition . body
  case strategy of
    NaiveRandom -> do
      logInfo "using 'random' strategy"
      randomStrategy stmt



    _ -> error "strategy not implemented"
  undefined
  -- initRandom
  -- let ast' = maskAsserts ast
  -- let inserters = runGen $ translationUnit ast'
  -- logInfo $ "insertion points: " <> display (length inserters)
  -- forM_ [(i,ins) | i <- [1.. iterations], ins <- inserters ] $ \(i,ins) -> do
  --     j <- liftIO (randomIO :: IO Int)
  --     let ast'' = runReader ins (notEqualsAssertion j)
  --         source = render $ pretty ast''
  --         prog = CProgram source program
  --     results <- executeVerifiers verifiers source
  --     -- persist the run
  --     persist' prog
  --     mapM_ persist' results


-- | executes n verifiers on 1 source file
executeVerifiers :: HasLogFunc env => [Verifier] -> String -> RIO env [VerifierRun]
executeVerifiers vs content =
  withSystemTempFile "input.c" $ \fp h -> do
        liftIO $ hPutStr h content >> hFlush h
        forM vs $ \v -> do
          env <- mkVerifierEnv (15 * 1000 * 1000) -- 15 seconds
          r <- runRIO env (execute v fp)
          return $ VerifierRun (verifierName v) r (hash content)


-- conclude :: [VerifierRun] -> Conclusion
-- conclude runs
--   | 1 <= acceptN && acceptN < rejectN = VerifiersUnsound (map runVerifierName accept)
--   | 1 <= rejectN && rejectN < acceptN = VerifiersIncomplete (map runVerifierName reject)
--   | acceptN == n                      = Agreement Unsat
--   | rejectN == n                      = Agreement Sat
--   | otherwise = Disagreement
--   where accept   = filter (\r -> verifierResult r == Unsat) runs
--         reject   = filter (\r -> verifierResult r == Sat) runs
--         acceptN  = length accept
--         rejectN  = length reject
--         n = length runs

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


-- -- TODO: check if seed is in the arguments
-- initRandom :: IO ()
-- initRandom = do
--       seed <- randomIO :: IO Int
--       let rnd = mkStdGen seed
--       putStrLn $ "seed: " <> show seed
--       setStdGen rnd

