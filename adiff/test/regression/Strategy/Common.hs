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

module Strategy.Common where

import           RIO

import           Control.Lens
import           Test.Tasty
import           Test.Tasty.HUnit

import           ADiff.Strategy.Common.Averages

testCommon :: TestTree
testCommon = testGroup "common" [testUpdateAverages]

testUpdateAverages :: TestTree
testUpdateAverages = testCase "average" $ do
  let avg0 = emptyAverages 3
  let avg1 = updateAverages [1.5,2,0] avg0
  assertEqual "average" (getAverages avg1) [1.5,2,0]
  let avg2 = updateAverages [1.5,4,1] avg1
  assertEqual "average" (getAverages avg2) [1.5,3,0.5]

