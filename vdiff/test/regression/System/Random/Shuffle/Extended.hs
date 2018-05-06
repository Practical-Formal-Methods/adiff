module System.Random.Shuffle.Extended where

import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           RIO
import           System.Random
import           System.Random.Shuffle
import           Test.Tasty
import           Test.Tasty.Hedgehog

testShuffle :: TestTree
testShuffle = localOption (Timeout 1000000 "1s") $ testGroup "shuffle" [ testLength
                                                                       -- , testEmpty
                                                                       ]


testLength = testProperty "length (shuffle x ) == length x" $ property $ do
  -- make a list
  let listLength = Range.linear 1 100
  (l :: [Int]) <- forAll $ Gen.list listLength Gen.enumBounded

  let stdGen = mkStdGen 1
  let shuffled = shuffle' l (length l) stdGen
  length shuffled === length l


-- testEmpty = testProperty "shuffle(0) terminates" $ property $ do
--   let empty = [] :: [Int]
--   let stdGen = mkStdGen 1
--   length ((shuffle' empty 1  stdGen :: [Int])) === 0
