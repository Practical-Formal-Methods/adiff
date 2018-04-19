module Strategy.Common where

import           RIO

import           Control.Lens
import           Control.Monad.State
import           Test.Tasty
import           Test.Tasty.HUnit

import           VDiff.Strategy.Common.Averages

testCommon :: TestTree
testCommon = testGroup "common" [testUpdateAverages]

testUpdateAverages :: TestTree
testUpdateAverages = testCase "average" $ void $ runStateT action initState
  where
    initState = SimpleAverageState [0,0,0] 0
    action = do
      updateAverages [1.5,2,0]
      avg1 <- use averages
      liftIO $ assertEqual "average" avg1 [1.5,2,0]
      updateAverages [1.5,4,1]
      avg2 <- use averages
      liftIO $ assertEqual "average" avg2 [1.5,3,0.5]

