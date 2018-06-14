module Main where

import           RIO
import           Test.Tasty

import           InstrumentationProperties
import           InstrumentationTest
import           Lens
import           TimedTest

import           Strategy.Common
import Persistence


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
    , pure testPersistence
    ]
  return $ testGroup "vdiff" tree


