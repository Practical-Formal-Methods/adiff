import           RIO
import           Test.Tasty

import           Spec.Instrumentation
import           Spec.Timed
import           Spec.Verifier

main :: IO ()
main = defaultMain  testEverything

testEverything :: TestTree
testEverything = testGroup "vdiff" [ testTimed
                                   , testVerifiers
                                   , testInstrumentation
                                   ]


