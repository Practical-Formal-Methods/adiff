{-# LANGUAGE TemplateHaskell #-}

module Main where

import           VDiff.Prelude

import           Data.FileEmbed
import qualified Data.Text        as T
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
testSimple = testGroup "unsat" $ map test [ (v, r) | v <- allVerifiers', r <- [Sat, Unsat], v ^. name /= "vim" ]
  where
    satFile     = $(embedFile "assets/test/sat.c")
    unsatFile     = $(embedFile "assets/test/unsat.c")
    allVerifiers' = filter (\v -> v ^. name /= "vim") allVerifiers

    test (v, expected) = testCase (T.unpack (v ^. name) ++ " " ++ show expected) $ do
      logOptions <- logOptionsHandle stderr True
      let logOptions' = setLogMinLevel LevelWarn $ setLogVerboseFormat False logOptions
      withLogFunc  logOptions' $ \lg -> do
        let verifierEnv   = VerifierEnv lg (15 * 1000 * 1000)
        runRIO verifierEnv $ withSystemTempFile "input.c" $ \fp h -> do
          BS.hPutStr h (if expected == Sat then satFile else unsatFile)
          hFlush h
          res <- execute v fp
          liftIO $ (res ^. verdict) @?= expected
