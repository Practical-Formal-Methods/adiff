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
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit


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
