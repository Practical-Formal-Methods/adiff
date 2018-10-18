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

{-# LANGUAGE TemplateHaskell #-}
module Util
  ( module Util
  , module Language.C
  , module Language.C.Analysis.TypeUtils
  , module Test.Tasty
  , module Test.Tasty.Golden
  , module Test.Tasty.HUnit
  ) where

import           ADiff.Prelude

import           Data.FileEmbed
import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.TravMonad
import           Language.C.Analysis.TypeUtils
import qualified RIO.ByteString.Lazy              as LBS
import           System.FilePath                  (replaceExtension)
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           ADiff.Instrumentation


dummyDecl :: CDeclaration SemPhase
dummyDecl = CDecl [] [] undefNode

dummyStmt :: String -> CStatement SemPhase
dummyStmt s = CExpr (Just var) (undefNode, voidType)
  where var = CVar (internalIdent s) (undefNode, voidType)


-- | Run golden tests
vsGoldenFile :: FilePath -> String -> (CTranslationUnit SemPhase-> IO LBS.ByteString) -> TestTree
vsGoldenFile fn name act = goldenVsString fn (replaceExtension fn ( "."  ++ name ++ "-golden" )) (openAndParse >>= act )
  where openAndParse = do
          c <- runRIO NoLogging $ openCFile fn
          case c of
            Nothing -> assertFailure $ "should be able to open, parse, and typecheck file" ++ fn
            Just ast -> return ast


simpleReads :: ByteString
simpleReads = $(embedOneFileOf  ["assets/test/reads/simple_reads.c"
                                , "adiff/assets/test/reads/simple_reads.c"
                                ])

parseAndAnalyseFile :: ByteString -> IO (CTranslationUnit SemPhase)
parseAndAnalyseFile bs =
  case parseC bs (initPos "nofilename") of
    Left _-> assertFailure "should be parseable"
    Right ast ->
      case runTrav_ (analyseAST ast) of
        Left _          -> assertFailure "should be typeable"
        Right (ast', _) -> return ast'
