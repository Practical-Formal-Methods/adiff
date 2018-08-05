module SimpleTypeCheck where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           VDiff.Prelude

import           Util

import           Data.Generics.Uniplate.Operations
import           Language.C.Analysis.SemRep
import           VDiff.Instrumentation
import           VDiff.SimpleTypecheck

testSimpleTypecheck :: IO TestTree
testSimpleTypecheck = do
  ts <- sequence [testAgainstDefault]
  return $ testGroup "simple typecheck" ts



testAgainstDefault :: IO TestTree
testAgainstDefault = do
  fs <- findByExtension [".c"] "assets/test/typecheck"
  return $ testGroup "against default" [runTest f | f <- fs]
  where
    runTest cf = testCase cf $ runRIO NoLogging $
      openCFile defaultTypechecker cf >>= \case
        Nothing -> traceM "okay, even the default cannot check it"
        Just defAst -> do
          (Just simpleAst) <- openCFile simpleTypechecker cf
          -- since we are only interested in expressions, we only compare expressions
          liftIO $ assertListEqualWith equalExpr (getExpressions defAst) (getExpressions simpleAst)

equalExpr :: CExpression SemPhase -> (String, Text)
equalExpr e = (prettyp e, simplifyType (getType e))
  where
    simplifyType (FunctionType (FunType ty _ _) _) = " * -> " <> tshow ty
    simplifyType (FunctionType (FunTypeIncomplete ty) _) = " * -> " <> tshow ty
    simplifyType x                                 = tshow x

getExpressions :: TU -> [CExpression SemPhase]
getExpressions = universeBi
