{-# LANGUAGE NoMonomorphismRestriction #-}

module Query {-# DEPRECATED "use Query2 instead" #-} (testQueries) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           PersistenceTestHelper

import           VDiff.Data
import           Database.Beam
import           VDiff.Persistence
import           VDiff.Prelude
import qualified VDiff.Query2                 as Q2


testQueries :: TestTree
testQueries = testGroup "Query"
  [ testAllFindings
  , testVerySimple
  , testRunIdWithVerdict
  , testIncompleteFindings
  , testUnsoundFindings
  ]
p1,p2 :: Program
p1 = Program "deadbeaf" "test.c"  "int main(){return 0;}"
p2 = Program "affe" "test2.c" "int main(char** c){return 1;}"

run1, run2, run3, run4, run5, run6 :: VerifierRun
run1 = VerifierRun 1 "verifier_uno"  (pk p1) satResult 0
run2 = VerifierRun 2 "verifier_due"  (pk p1) unsatResult 0
run3 = VerifierRun 3 "verifier_tres" (pk p1) unknownResult 0
run4 = VerifierRun 4 "verifier_quat" (pk p1) satResult 0
run5 = VerifierRun 5 "verifier_cinq" (pk p2) satResult 0
run6 = VerifierRun 6 "verifier_sei"  (pk p1) unsatResult 0

satResult, unsatResult, unknownResult :: VerifierResult
satResult     = VerifierResult (Just 1.5) (Just 25343) Sat
unsatResult   = VerifierResult (Just 1.5) (Just 25343) Unsat
unknownResult = VerifierResult Nothing Nothing Unknown

-- | this tests whether the marshalling of the custom "verdict" type to SQL values works.
testVerySimple :: TestTree
testVerySimple = testCase "simple" $ withTestEnv $ do
  runBeam migrateVdiff
  runBeam Q2.updateCountsTable
  runBeam $ runInsert $ insert (vdiffDb ^. runs) $ insertValues [run1, run2, run3]
  someRuns <- runBeam $ runSelectReturningList $ select q
  liftIO $ someRuns @?= [run1]
  where
    q = filter_ (\r -> (r ^. (result . verdict)) ==. val_ Sat) Q2.allRuns_

testRunIdWithVerdict :: TestTree
testRunIdWithVerdict = testCase "testRunIdWithVerdict" $ withTestEnv $ do
  runBeam migrateVdiff
  runBeam Q2.updateCountsTable
  runBeam $ runInsert $ insert (vdiffDb ^. programs) $ insertValues [p1, p2]
  runBeam$ runInsert $ insert (vdiffDb ^. runs) $ insertValues [run1, run2, run3, run4,run5]
  sats <- runBeam $ runSelectReturningList $ select $ Q2.runIdWithVerdict Sat
  liftIO $ sats @?= [(1,2),(2,2),(3,2),(4,2),(5,1)]
  unsats <- runBeam $ runSelectReturningList $ select $ Q2.runIdWithVerdict Unsat
  liftIO $ unsats @?= [(1,1),(2,1),(3,1),(4,1)]

testAllFindings :: TestTree
testAllFindings = testCase "allFindings" $ withTestEnv $ do
  runBeam $ do
    migrateVdiff
    runInsert $ insert (vdiffDb ^. programs) $ insertValues [p1, p2]
    runInsert $ insert (vdiffDb ^. runs) $ insertValues [run1, run2, run3, run4, run5]
    Q2.updateCountsTable
  fs <- runBeam $ runSelectReturningList $ select Q2.allFindings
  liftIO $ fs @?= [ (run1, Just "test.c", 2, 1)
                  , (run2, Just "test.c", 2, 1)
                  , (run3, Just "test.c", 2, 1)
                  , (run4, Just "test.c", 2, 1)
                  , (run5, Just "test2.c", 1, 0) ]

testIncompleteFindings :: TestTree
testIncompleteFindings = testCase "incompleteFindings" $ withTestEnv $ do
  runBeam $ do
    migrateVdiff
    runInsert $ insert (vdiffDb ^. programs) $ insertValues [p1, p2]
    runInsert $ insert (vdiffDb ^. runs) $ insertValues [run1, run2, run3, run5, run6]
    Q2.updateCountsTable
  fs <- runBeam $ runSelectReturningList $ select Q2.incompleteFindings
  liftIO $ fs @?= [(run1, Just "test.c", 1, 2)]

testUnsoundFindings :: TestTree
testUnsoundFindings = testCase "unsoundFindings" $ withTestEnv $ do
  runBeam $ do
    migrateVdiff
    runInsert $ insert (vdiffDb ^. programs) $ insertValues [p1, p2]
    runInsert $ insert (vdiffDb ^. runs) $ insertValues [run1, run2, run3, run4, run5]
    Q2.updateCountsTable
  fs <- runBeam $ runSelectReturningList $ select Q2.unsoundFindings
  liftIO $ fs @?= [(run2, Just "test.c", 2, 1)]
