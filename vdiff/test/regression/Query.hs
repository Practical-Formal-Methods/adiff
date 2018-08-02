{-# LANGUAGE NoMonomorphismRestriction #-}

module Query (testQueries) where

import           Data.List             (repeat, sort)
import           PersistenceTestHelper
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit

import           Util

import           Database.Beam
import           VDiff.Data
import           VDiff.Persistence
import           VDiff.Prelude
import qualified VDiff.Query2          as Q2



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
  runBeam migrateVdiff
  runBeam $ runInsert $ insert (vdiffDb ^. runs) $ insertValues [run1, run2, run3]
  someRuns <- runBeam $ runSelectReturningList $ select q
  liftIO $ someRuns @?= [run1]
  where
    q = filter_ (\r -> (r ^. (result . verdict)) ==. val_ Sat) Q2.allRuns_

testAllFindings :: TestTree
testAllFindings = testCase "allFindings" $ withTestEnv $ do
  runBeam $ do
    migrateVdiff
    runInsert $ insert (vdiffDb ^. programs) $ insertValues [p1, p2]
    runInsert $ insert (vdiffDb ^. runs) $ insertValues [run1, run2, run2b, run3, run4, run5]
  fs <- Q2.executeQuery 20 0 Q2.Everything
  let expected = [ Q2.Finding (pk p1) "test.c" 2 1 ["verifier_quat", "verifier_uno"] ["verifier_due"]
                 , Q2.Finding (pk p2) "test2.c" 1 0 ["verifier_cinq"] []
                 ]
  liftIO $ assertListEqual (sort expected) (sort fs)


testIncompleteFindings :: TestTree
testIncompleteFindings = testCase "incompleteFindings" $ withTestEnv $ do
  runBeam $ do
    migrateVdiff
    runInsert $ insert (vdiffDb ^. programs) $ insertValues [p1, p2]
    runInsert $ insert (vdiffDb ^. runs) $ insertValues [run1, run2, run3, run5, run6]
  vns <-  Q2.getVerifierNames
  let weights = Weights [(v, 1) | v <- vns]
  fs <- Q2.executeQuery 20 0 $  Q2.Query Q2.SuspicionIncomplete Nothing (ConsensusBy weights)
  liftIO $ fs @?= [ Q2.Finding (pk p1) "test.c" 1 2 ["verifier_uno"] ["verifier_due", "verifier_sei"] ]


testUnsoundFindings :: TestTree
testUnsoundFindings = testCase "unsoundFindings" $ withTestEnv $ do
  runBeam $ do
    migrateVdiff
    runInsert $ insert (vdiffDb ^. programs) $ insertValues [p1, p2]
    runInsert $ insert (vdiffDb ^. runs) $ insertValues [run1, run2, run3, run4, run5]
  vns <-  Q2.getVerifierNames
  let weights = Weights [(v, 1) | v <- vns]
  fs <- Q2.executeQuery 20 0 $  Q2.Query Q2.SuspicionUnsound Nothing (ConsensusBy weights)
  liftIO $ fs @?= [ Q2.Finding (pk p1) "test.c" 2 1 ["verifier_quat", "verifier_uno"] ["verifier_due"] ]
