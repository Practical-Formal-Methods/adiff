import           RIO
import           Test.Tasty

import           Spec.Instrumentation
import           Spec.Timed
import           Spec.Verifier

main :: IO ()
main = do
  tree <- constructTree
  defaultMain  tree

constructTree :: IO TestTree
constructTree = do
  tree <- sequence
    [ pure testTimed
    , pure testVerifiers
    , testInstrumentation
    ]
  return $ testGroup "vdiff" tree


