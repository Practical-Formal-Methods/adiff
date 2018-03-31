module Main where

import           RIO
import           Test.Tasty

import           InstrumentationTest
import           Lens
import           TimedTest

main :: IO ()
main = do
  tree <- constructTree
  defaultMain  tree

constructTree :: IO TestTree
constructTree = do
  tree <- sequence
    [ pure testTimed
    , testInstrumentation
    , pure testLenses
    ]
  return $ testGroup "vdiff" tree


