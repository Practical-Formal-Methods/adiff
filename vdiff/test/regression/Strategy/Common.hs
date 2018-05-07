module Strategy.Common where

import           RIO

import           Control.Lens
import           Test.Tasty
import           Test.Tasty.HUnit

import           VDiff.Strategy.Common.Averages

testCommon :: TestTree
testCommon = testGroup "common" [testUpdateAverages]

testUpdateAverages :: TestTree
testUpdateAverages = testCase "average" $ do
  let avg0 = emptyAverages 3
  let avg1 = updateAverages [1.5,2,0] avg0
  assertEqual "average" (getAverages avg1) [1.5,2,0]
  let avg2 = updateAverages [1.5,4,1] avg1
  assertEqual "average" (getAverages avg2) [1.5,3,0.5]

