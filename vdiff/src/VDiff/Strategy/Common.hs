{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | common things that are necessary to implement strategies.
module VDiff.Strategy.Common
  ( module VDiff.Strategy.Common
  , module VDiff.Strategy.Common.ConstantPool
  , Ident
  , Type
  ) where

import           VDiff.Prelude

import           Control.Lens
import           Control.Monad.State
import           Language.C
import           Language.C.Analysis.SemRep         hiding (Stmt)
import           Language.C.Analysis.TypeUtils
import           Safe
import           System.IO                          (hPutStr)
import           System.Random
import           Text.PrettyPrint.HughesPJ          (render)


import           VDiff.Data
import           VDiff.Instrumentation
import           VDiff.Instrumentation.Reads
import           VDiff.Persistence
import           VDiff.Strategy.Common.ConstantPool
import VDiff.Strategy.Common.Budget

class (HasTranslationUnit env, HasLogFunc env, HasDiffParameters env) => StrategyEnv env


verifyB :: (IsStrategyEnv env, MonadReader env m, MonadIO m, MonadBudget m)
  => CTranslationUnit SemPhase
  -> m ([VerifierRun], Conclusion)
verifyB tu = budgeted (verify tu)


verify :: (IsStrategyEnv env, MonadReader env m, MonadIO m) => CTranslationUnit SemPhase -> m ([VerifierRun], Conclusion)
verify tu = do
  (prog, res) <- verify' tu
  let conclusion = conclude res
  case conclusion of
    Unsoundness _ -> logInfo $ "found unsoundness with program " <> display (prog ^. hash)
    Incompleteness _ -> logInfo $ "found incompleteness with program " <> display (prog ^. hash)
    _ -> return ()
  return (res, conclusion)

-- | runs the given translation unit against the configured verifiers.
verify' :: (IsStrategyEnv env, MonadReader env m, MonadIO m) => CTranslationUnit SemPhase -> m (CProgram, [VerifierRun])
verify' tu = do
  vs <- view (diffParameters . verifiers)
  time <- view (diffParameters . timelimit)
  env <- ask
  runRIO env $ withSystemTempFile "input.c" $ \fp h -> do
        -- write file
        originalFileName <- view (diffParameters . program)
        let content = render . pretty $ tu
            program' = CProgram content originalFileName (mkHash content)
        persist' program'
        liftIO $ hPutStr h content >> hFlush h
        -- run each verifier
        runs <- forM vs $ \v -> do
                vEnv <- mkVerifierEnv time
                r <- runRIO vEnv $ execute v fp
                let run = VerifierRun (verifierName v) r (program' ^. hash)
                persist' run
                return run
        return (program', runs)


conclude :: [VerifierRun] -> Conclusion
conclude  rs = if
  | all (\r -> verdict (verifierResult r) == Sat) rs                  -> StrongAgreement Sat
  | all (\r -> verdict (verifierResult r) == Unsat) rs                -> StrongAgreement Unsat
  | all (\r -> verdict (verifierResult r) `elem` [Sat, Unknown]) rs   -> WeakAgreement Sat
  | all (\r -> verdict (verifierResult r) `elem` [Unsat, Unknown]) rs -> WeakAgreement Unsat
  | length sats > length unsats && not (null unsats)                  -> Unsoundness unsats
  | length unsats > length sats && not (null sats)                    -> Incompleteness sats
  | otherwise -> Disagreement
  where
    unsats = [ runVerifierName r | r <- rs, verdict (verifierResult r) == Unsat ]
    sats =   [ runVerifierName r | r <- rs, verdict (verifierResult r) == Sat ]

mkRandomAssertion :: (MonadRandom m)  => CExpression SemPhase -> m Stmt
mkRandomAssertion e = do
      let ty = getType e
      constv <-mkRandomConstant ty
      let constant'  = CConst constv
          expression = CBinary CNeqOp e constant' (undefNode, boolType)
      return (assertStmt expression)

mkAssertionFromPool :: (MonadRandom m) => CExpression SemPhase -> ConstantPool -> m (Maybe Stmt)
mkAssertionFromPool e pool = do
  let ty = getType e
  let cs = lookupPool ty pool
  chooseOneOf cs >>= \case
    Nothing -> return Nothing
    Just c' -> do
      let cnst = CConst c'
          expr = CBinary CNeqOp e cnst (undefNode, voidType)
      return $ Just $ assertStmt expr


mkRandomConstant :: (MonadRandom m) => Type -> m (CConstant SemPhase)
mkRandomConstant ty
  | ty `sameType` integral TyChar = do
      (c :: Char) <- getRandom
      return $ CCharConst (CChar c False) (undefNode, ty)

  | ty `sameType` integral TyBool = do
    (b :: Bool) <- getRandom
    let v = if b then 1 else 0
    return $ CIntConst (cInteger v) (undefNode, ty)

  | ty `sameType` integral TyUInt = do
      (v :: Int32) <- uniformLengthInt
      return $ CIntConst (cInteger $ fromIntegral (abs v))  (undefNode, ty)

  | otherwise = do
      (v :: Int32) <- uniformLengthInt
      return $ CIntConst (cInteger $ fromIntegral v) (undefNode, ty)

-- | produces a random int32, where the bitwidth is uniformly distributed between 0 and 32
uniformLengthInt :: (MonadRandom m) => m Int32
uniformLengthInt = do
  width <- getRandomR (1,32)
  bits <- take width <$> getRandoms
  return $ bitsToNum $! bits
  where
    bitsToNum :: [Bool] -> Int32
    bitsToNum = foldl' f 0
    f n False = 2 * n
    f n True  = 2 * n + 1

assertStmt :: CExpression SemPhase -> CStatement SemPhase
assertStmt expr = CExpr (Just $ CCall identifier [expr] (undefNode, voidType)) (undefNode, voidType)
  where
    identifier = CVar (builtinIdent "__VERIFIER_assert") (undefNode,voidType)

-- | assertUnequal e c results in an AST fragment with the assertion @e != c@
assertUnequal :: CExpression SemPhase -> CConstant SemPhase -> CStatement SemPhase
assertUnequal expr c = assertStmt $ expr `unequalC`  (CConst c)


-- | assertUnequals e c1, c2,.. results in an AST fragment with the assertion @e
-- != c1 && e != c2 ...@
assertUnequals :: CExpression SemPhase -> [CConstant SemPhase] -> CStatement SemPhase
assertUnequals expr []     = assertStmt constantTrue
assertUnequals expr (c:cs) = assertStmt $ foldl' f (expr `unequalC` (CConst c)) cs
  where
    f e c            = e `andC` (CBinary CNeqOp expr (CConst c) (undefNode, boolType))



-- | a simple 'assert(false)' statement
assertFalse :: CStatement SemPhase
assertFalse = assertStmt constantFalse

constantTrue = CConst $ CIntConst (cInteger 0) (undefNode, voidType)
constantFalse = CConst $ CIntConst (cInteger 1) (undefNode, voidType)

-- little shortcuts
andC, unequalC :: CExpression SemPhase -> CExpression SemPhase -> CExpression SemPhase
e1 `andC` e2     = CBinary CLndOp e1 e2 (undefNode, boolType)
e1 `unequalC` e2 = CBinary CNeqOp e1 e2 (undefNode, boolType)


currentReads :: (MonadBrowser m, MonadReader env m, HasDiffParameters env) => m [CExpression SemPhase]
currentReads = do
  m <- view (diffParameters . searchMode)
  stmt <- currentStmt
  return $ readStatement m stmt
