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

import           Control.Lens                       hiding ((^.))
import           Control.Monad.State
import           Language.C
import           Language.C.Analysis.SemRep         hiding (Stmt)
import           Language.C.Analysis.TypeUtils
import           RIO                                hiding (view)
import           Safe
import           System.IO                          (hPutStr)
import           System.Random
import           Text.PrettyPrint.HughesPJ          (render)



import           VDiff.Data
import           VDiff.Instrumentation
import           VDiff.Persistence
import           VDiff.Strategy.Common.ConstantPool
import           VDiff.Types

class (HasTranslationUnit env, HasLogFunc env, HasDiffParameters env) => StrategyEnv env


isCompound ::Stmt -> Bool
isCompound (CCompound _ _ _ ) = True
isCompound _                  = False

verify :: (IsStrategyEnv env, Monad m, MonadReader env m, MonadIO m, MonadState st m, HasBudget st Int) => CTranslationUnit SemPhase -> m ([VerifierRun], Conclusion)
verify tu = do
  (prog, res) <- verify' tu
  let conclusion = conclude res
  case conclusion of
    Unsoundness _ -> logInfo $ "found unsoundness with program " <> display (prog ^. hash)
    Incompleteness _ -> logInfo $ "found incompleteness with program " <> display (prog ^. hash)
    _ -> return ()
  return (res, conclusion)

-- | runs the given translation unit against the configured verifiers.
verify' :: (IsStrategyEnv env, Monad m, MonadReader env m, MonadIO m, MonadState st m, HasBudget st Int) => CTranslationUnit SemPhase -> m (CProgram, [VerifierRun])
verify' tu = do
  budget -= 1
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
                env <- mkVerifierEnv time
                r <- runRIO env $ execute v fp
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

mkAssertion :: (MonadIO m ) => Ident -> Type -> m Stmt
mkAssertion varName ty = do
      constv <- if | ty `sameType` integral TyChar -> do
                      (c :: Char) <- liftIO randomIO
                      return $ CCharConst (CChar c False) (undefNode, ty)
                   | ty `sameType` integral TyBool -> do
                      (b :: Bool) <- liftIO randomIO
                      let v = if b then 1 else 0
                      return $ CIntConst (cInteger v) (undefNode, ty)
                   | ty `sameType` integral TyUInt -> do
                       (v :: Int32) <- liftIO randomIO
                       return $ CIntConst (cInteger $ fromIntegral (abs v))  (undefNode, ty)
                   | otherwise -> do
                       (v :: Int32) <- liftIO randomIO
                       return $ CIntConst (cInteger $ fromIntegral v) (undefNode, ty)
      let  constant'   = CConst constv
           var        = CVar varName (undefNode, ty)
           expression = CBinary CNeqOp var constant' (undefNode, boolType)
      return (assertStmt expression)

assertStmt :: CExpression SemPhase -> CStatement SemPhase
assertStmt expr = CExpr (Just $ CCall identifier [expr] (undefNode, voidType)) (undefNode, voidType)
  where
    identifier = CVar (builtinIdent "__VERIFIER_assert") (undefNode,voidType)


-- | a simple 'assert(false)' statement
assertFalse :: CStatement SemPhase
assertFalse = assertStmt false
  where
    false = CConst $ CIntConst (cInteger 0) voidNode
    voidNode = (undefNode, voidType)

chooseOneOf :: (MonadIO m) => [a] ->  m (Maybe a)
chooseOneOf options = do
  i <- liftIO $ getStdRandom $ randomR (0, length options - 1)
  return (options `atMay` i)
