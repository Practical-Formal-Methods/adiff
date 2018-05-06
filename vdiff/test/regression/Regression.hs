module Main where

import           RIO
import           Test.Tasty

import           InstrumentationProperties
import           InstrumentationTest
import           Lens
import           System.Random.Shuffle.Extended
import           TimedTest

import           Strategy.Common


main :: IO ()
main = do
  tree <- constructTree
  defaultMain  tree

constructTree :: IO TestTree
constructTree = do
  tree <- sequence
    [ pure testTimed
    , testInstrumentation
    , pure testInstrumentationProperties
    , testLenses
    , pure testCommon
    , pure testShuffle
    ]
  return $ testGroup "vdiff" tree


