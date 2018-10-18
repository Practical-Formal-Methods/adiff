-- MIT License
--
-- Copyright (c) 2018 Christian Klinger
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module TimedTest where

import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit
import           ADiff.Prelude

import           ADiff.Timed

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
  x <- exec ( proc "bash" ["-c", "echo 'foo\nbar'"] ) True "" (fromSeconds 1)
  case x of
    (Nothing, _, _) -> assertFailure "this should not run into a timeout"
    (Just (code,_) , out, _)  -> do
      code @?= ExitSuccess
      out @?= "foo\nbar\n"


testStderr :: TestTree
testStderr =  testCase "echo (stderr)" $ do
    x <- exec (proc "bash" ["-c", "(>&2 echo 'foo\nbar')"]) True "" (fromSeconds 1)
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
      x <- exec cp  True "foobar\n" (fromSeconds 1)
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
      x <- exec cp True "" (Timelimit 400000)
      case x of
        (Just c, _, _)  -> assertFailure $ "unexpected error code " <> show c
        (Nothing, _, _) -> return ()
