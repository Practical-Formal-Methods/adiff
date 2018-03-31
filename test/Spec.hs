import           RIO
import           Test.Tasty

import           Spec.Instrumentation
import           Spec.Timed
import           Spec.Verifier
import           Spec.Lens

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
    , pure testLenses
    ]
  return $ testGroup "vdiff" tree


