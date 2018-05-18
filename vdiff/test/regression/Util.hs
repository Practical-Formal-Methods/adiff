{-# LANGUAGE TemplateHaskell #-}
module Util
  ( module Util
  , module Language.C
  , module Language.C.Analysis.TypeUtils
  , module Test.Tasty
  , module Test.Tasty.Golden
  , module Test.Tasty.HUnit
  ) where

import           Data.FileEmbed
import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.TravMonad
import           Language.C.Analysis.TypeUtils
import           RIO
import qualified RIO.ByteString.Lazy              as LBS
import           System.FilePath                  (replaceExtension)
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           VDiff.Instrumentation
import           VDiff.Types

data NoLogging = NoLogging
instance HasLogFunc NoLogging where
  logFuncL = lens getter setter
    where logFunc = mkLogFunc (\_ _ _ _ -> return ())
          getter = const logFunc
          setter = const $ const NoLogging

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
                                , "vdiff/assets/test/reads/simple_reads.c"
                                ])

parseAndAnalyseFile :: ByteString -> IO (CTranslationUnit SemPhase)
parseAndAnalyseFile bs =
  case parseC bs (initPos "nofilename") of
    Left _-> assertFailure "should be parseable"
    Right ast ->
      case runTrav_ (analyseAST ast) of
        Left _          -> assertFailure "should be typeable"
        Right (ast', _) -> return ast'
