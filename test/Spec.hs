import           RIO
import qualified RIO.Text         as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Timed

import           System.Process

main :: IO ()
main = defaultMain testEverything

testEverything :: TestTree
testEverything = testGroup "timed"
  [ testStdout
  , testStderr
  , testStdin
  , testTermination
  ]

testStdout:: TestTree
testStdout =  testCase "echo (stdout)" $ do
    x <- exec ( shell "echo 'foo\nbar'" ) "" (1*1000*1000)
    case x of
      Nothing -> assertFailure "this should not run into a timeout"
      Just (code, out, _)  -> do
        code @?= ExitSuccess
        out @?= "foo\nbar\n"

testStderr :: TestTree
testStderr =  testCase "echo (stderr)" $ do
    x <- exec ( shell "(>&2 echo 'foo\nbar')") "" (1*1000*1000)
    case x of
      Nothing -> assertFailure "this should not run into a timeout"
      Just (code, _, err)  -> do
        code @?= ExitSuccess
        err @?= "foo\nbar\n"

testStdin :: TestTree
testStdin =  testCase "read/echo (stdin)" $ do
    x <- exec (shell "read X; echo $X;") "foobar\n" (1*1000*1000)
    case x of
      Nothing -> assertFailure "this should not run into a timeout"
      Just (code, out, _)  -> do
        code @?= ExitSuccess
        out @?= "foobar\n"

testTermination :: TestTree
testTermination =  testCase "terminates bash-loop" $ do
    x <- exec (shell "sleep infinity") "" (1*1000*1000)
    case x of
      Just _  -> assertFailure "this should not have terminated"
      Nothing -> return ()





