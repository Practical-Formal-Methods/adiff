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

module Lens where

import           ADiff.Prelude
import           Util

import           Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as LC8

import           Language.C.Data.Lens
import           ADiff.Instrumentation

testLenses :: IO TestTree
testLenses = testGroup "lenses" <$> sequence [ pure testIndex
                                             , testDefinedFunctions
                                             ]

testIndex :: TestTree
testIndex = goldenVsString "index main" "assets/test/lenses/functions.c.golden" act
  where act = do
          (Just ast) <- runRIO NoLogging $ openCFile "assets/test/lenses/functions.c"
          let dummyBody = CCompound [] [CBlockStmt (dummyStmt "dummy")] (undefNode, voidType)
              ast' = (ix "main" . functionDefinition . body ) .~ dummyBody $ ast
          return $ LC8.pack $ prettyp ast'

testDefinedFunctions :: IO TestTree
testDefinedFunctions = testGroup "defined functions" . map runTest <$> findByExtension [".c"] "assets/test/reads"
  where
    runTest cf = vsGoldenFile cf "defined-functions" $ return . LC8.pack . show . map identToString . definedFunctions

