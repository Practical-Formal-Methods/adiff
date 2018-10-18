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

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ParallelListComp          #-}

module Query (testQueries) where

import           PersistenceTestHelper
import           Test.Tasty
import           Test.Tasty.HUnit
import Data.List (sort, repeat)
import System.IO

import           Database.Beam
import           ADiff.Data
import           ADiff.Persistence
import           ADiff.Prelude
import qualified ADiff.Query2          as Q2


assertListEqual :: HasCallStack => (Eq a, Show a) => [a] -> [a] -> IO ()
assertListEqual l r
  | length l == length r = sequence_ [assertEqual ("at index " ++ show i ++ " equal") x y | x <- l | y <- r | i <- [0..]]
  | otherwise = assertFailure "list of unequal length cannot be equal"

testQueries :: TestTree
testQueries = testGroup "Query"
  [ testAllFindings
  , testVerySimple
  , testIncompleteFindings
  , testUnsoundFindings
  ]
p1,p2 :: Program
p1 = Program "deadbeaf" "test.c"  "int main(){return 0;}"
p2 = Program "affe" "test2.c" "int main(char** c){return 1;}"

run1, run2, run3, run4, run5, run6 :: VerifierRun
run1  = VerifierRun 1 "verifier_uno"  (pk p1) satResult 0
run2  = VerifierRun 2 "verifier_due"  (pk p1) unsatResult 0
run2b = VerifierRun 20 "verifier_due" (pk p1) unsatResult 0
run3  = VerifierRun 3 "verifier_tres" (pk p1) unknownResult 0
run4  = VerifierRun 4 "verifier_quat" (pk p1) satResult 0
run5  = VerifierRun 5 "verifier_cinq" (pk p2) satResult 0
run6  = VerifierRun 6 "verifier_sei"  (pk p1) unsatResult 0

satResult, unsatResult, unknownResult :: VerifierResult
satResult     = VerifierResult (Just 1.5) (Just 25343) Sat
unsatResult   = VerifierResult (Just 1.5) (Just 25343) Unsat
unknownResult = VerifierResult Nothing Nothing Unknown

-- | this tests whether the marshalling of the custom "verdict" type to SQL values works.
testVerySimple :: TestTree
testVerySimple = testCase "simple" $ withTestEnv $ do
  runBeam migrateAdiff
  runBeam $ runInsert $ insert (adiffDb ^. runs) $ insertValues [run1, run2, run3]
  someRuns <- runBeam $ runSelectReturningList $ select q
  liftIO $ someRuns @?= [run1]
  where
    q = filter_ (\r -> (r ^. (result . verdict)) ==. val_ Sat) Q2.allRuns_

testAllFindings :: TestTree
testAllFindings = testCase "allFindings" $ withTestEnv $ do
  runBeam $ do
    migrateAdiff
    runInsert $ insert (adiffDb ^. programs) $ insertValues [p1, p2]
    runInsert $ insert (adiffDb ^. runs) $ insertValues [run1, run2, run2b, run3, run4, run5]
  fs <- Q2.executeQuery 20 0 Q2.Everything
  let expected = [ Q2.Finding (pk p1) "test.c" 2 1 ["verifier_quat", "verifier_uno"] ["verifier_due"]
                 , Q2.Finding (pk p2) "test2.c" 1 0 ["verifier_cinq"] []
                 ]
  liftIO $ assertListEqual (sort expected) (sort fs)

testIncompleteFindings :: TestTree
testIncompleteFindings = testCase "incompleteFindings" $ withTestEnv $ do
  runBeam $ do
    migrateAdiff
    runInsert $ insert (adiffDb ^. programs) $ insertValues [p1, p2]
    runInsert $ insert (adiffDb ^. runs) $ insertValues [run1, run2, run3, run5, run6]
  vns <-  Q2.getVerifierNames
  let weights = Weights SimpleBinaryMajority [(v, 1) | v <- vns]
  fs <- Q2.executeQuery 20 0 $  Q2.Query Q2.SuspicionIncomplete Nothing (ConsensusBy weights)
  liftIO $ fs @?= [ Q2.Finding (pk p1) "test.c" 1 2 ["verifier_uno"] ["verifier_due", "verifier_sei"] ]


testUnsoundFindings :: TestTree
testUnsoundFindings = testCase "unsoundFindings" $ withTestEnv $ do
  runBeam $ do
    migrateAdiff
    runInsert $ insert (adiffDb ^. programs) $ insertValues [p1, p2]
    runInsert $ insert (adiffDb ^. runs) $ insertValues [run1, run2, run3, run4, run5]
  vns <-  Q2.getVerifierNames
  let weights = Weights SimpleBinaryMajority [(v, 1) | v <- vns]
  fs <- Q2.executeQuery 20 0 $  Q2.Query Q2.SuspicionUnsound Nothing (ConsensusBy weights)
  liftIO $ fs @?= [ Q2.Finding (pk p1) "test.c" 2 1 ["verifier_quat", "verifier_uno"] ["verifier_due"] ]
