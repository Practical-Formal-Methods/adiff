module Spec.Util
  ( module Spec.Util
  , module Language.C
  , module Language.C.Analysis.TypeUtils
  , module Test.Tasty
  , module Test.Tasty.Golden
  , module Test.Tasty.HUnit
  ) where

import           Language.C
import           Language.C.Analysis.TypeUtils
import           RIO
import qualified RIO.ByteString.Lazy           as LBS
import           System.FilePath               (replaceExtension)
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Instrumentation

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
vsGoldenFile fn name act = goldenVsString name (replaceExtension fn ".golden") (openAndParse >>= act )
  where openAndParse = do
          c <- runRIO NoLogging $ openCFile fn
          case c of
            Nothing -> assertFailure $ "should be able to open, parse, and typecheck file" ++ fn
            Just ast -> return ast
