{-# LANGUAGE TemplateHaskell #-}

module Main where

import           VDiff.Prelude

import           Data.FileEmbed
import qualified RIO.ByteString   as BS
import           Test.Tasty
import           Test.Tasty.HUnit

import           VDiff.Data
import           VDiff.Verifier   (allVerifiers)

main :: IO ()
main = defaultMain  $ testGroup "vdiff-integration" [testVerifiers]


testVerifiers :: TestTree
testVerifiers = testGroup "verifiers" [testSimple]


testSimple :: TestTree
testSimple = testGroup "unsat" $ map test [ (v, r) | v <- allVerifiers', r <- [Sat, Unsat], verifierName v /= "vim" ]
  where
    satFile     = $(embedFile "assets/test/sat.c")
    unsatFile     = $(embedFile "assets/test/unsat.c")
    allVerifiers' = filter (\v -> verifierName v /= "vim") allVerifiers

    test (v, expected) = testCase (verifierName v ++ " " ++ show expected) $ do
      logOptions <- logOptionsHandle stderr True
      let logOptions' = setLogMinLevel LevelWarn $ setLogVerboseFormat False logOptions
      withLogFunc  logOptions' $ \lg -> do
        let verifierEnv   = VerifierEnv lg (15 * 1000 * 1000)
        runRIO verifierEnv $ withSystemTempFile "input.c" $ \fp h -> do
          BS.hPutStr h (if expected == Sat then satFile else unsatFile)
          hFlush h
          res <- execute v fp
          liftIO $ verdict res @?= expected
