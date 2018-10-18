-- MIT License
--
-- Copyright (c) 2018 Christian Klinger
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE TemplateHaskell #-}

module Main where

import           ADiff.Prelude

import           Data.FileEmbed
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Docker.Client      as Docker
import qualified RIO.ByteString     as BS
import           Test.Tasty
import           Test.Tasty.HUnit

import           ADiff.Data
import           ADiff.Execute
import           ADiff.Verifier     (allVerifiers)

main :: IO ()
main = defaultMain  $ testGroup "adiff-integration" [testVerifiers]


testVerifiers :: TestTree
testVerifiers = testGroup "verifiers" [testSimple]


testSimple :: TestTree
testSimple = testGroup "unsat" $ map test [ (v, r) | v <- allVerifiers', r <- [Sat, Unsat], v ^. name /= "vim" ]
  where
    satFile       = $(embedFile "assets/test/sat.c")
    unsatFile     = $(embedFile "assets/test/unsat.c")
    allVerifiers' = filter (\v -> v ^. name /= "vim") allVerifiers

    test (v, expected) = testCase (T.unpack (v ^. name) ++ " " ++ show expected) $ do
      logOptions <- logOptionsHandle stderr True
      let logOptions' = setLogMinLevel LevelWarn $ setLogVerboseFormat False logOptions
      withLogFunc  logOptions' $ \lg -> do
        let testFile = if expected == Sat then satFile else unsatFile
        res <- runRIO lg $ executeVerifierInDocker (defaultVerifierResources (fromSeconds 30)) (v ^. name) [] (decodeUtf8 testFile)
        (res ^. verdict) @?= expected
