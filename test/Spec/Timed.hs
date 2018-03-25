module Spec.Timed where

import           RIO
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit

import           Timed

testTimed :: TestTree
testTimed = localOption (Timeout 6000000 "6s") $ testGroup "timed"
  [ testStdoutN 1
  , testStderr
  , testStdin
  , testTermination
  , testStdoutN 100
  ]

testStdoutN :: Int -> TestTree
testStdoutN n =  testCase ("echo (stdout) n=" ++ show n) (replicateM_ n testStdout')

testStdout' :: IO ()
testStdout' = do
  x <- exec ( proc "bash" ["-c", "echo 'foo\nbar'"] ) "" (1*1000*1000)
  case x of
    (Nothing, _, _) -> assertFailure "this should not run into a timeout"
    (Just (code,_) , out, _)  -> do
      code @?= ExitSuccess
      out @?= "foo\nbar\n"


testStderr :: TestTree
testStderr =  testCase "echo (stderr)" $ do
    x <- exec (proc "bash" ["-c", "(>&2 echo 'foo\nbar')"]) "" (1*1000*1000)
    case x of
      (Nothing, _, _) -> assertFailure "unexpected timeout"
      (Just (code,_), out, err)  -> do
        code @?= ExitSuccess
        out @?= ""
        err @?= "foo\nbar\n"

testStdin :: TestTree
testStdin =  testGroup "read/echo (stdin)" $
  map test [ ("via proc", proc "bash" ["-c", "read X; echo $X;"])
           , ("via shell", shell "read X; echo $X;")
           ]
  where
    test (name, cp) = testCase name $ do
      x <- exec cp  "foobar\n" (1*1000*1000)
      case x of
        (Nothing, _, _) -> assertFailure "this should not run into a timeout"
        (Just (code,_), out, err)  -> do
          code @?= ExitSuccess
          out @?= "foobar\n"
          err @?= ""

testTermination :: TestTree
testTermination =  testGroup "terminates bash-loop" $
  map test [ ("via proc", proc "bash" ["-c", "sleep infinity"])
           , ("via shell", shell "sleep infinity")
           ]
  where
    test (name, cp) = testCase name $ do
      x <- exec cp "" (400*1000)
      case x of
        (Just c, _, _)  -> assertFailure $ "unexpected error code " <> show c
        (Nothing, _, _) -> return ()
