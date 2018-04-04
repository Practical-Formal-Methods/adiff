{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Strategy.Random (randomStrategy) where

import qualified Prelude                       as P
import           RIO

import           Control.Lens.Operators
import           Language.C
import           Language.C.Analysis.SemRep    hiding (Stmt)
import           Language.C.Analysis.TypeUtils
import           Language.C.Data.Lens
import           System.Random

import           Data
import           Instrumentation
import           Types

import           Strategy.Util

-- does not terminate
randomStrategy :: (HasLogFunc env, HasTranslationUnit env, HasDiffParameters env) => RIO env ()
randomStrategy = do
  tu <- view translationUnit
  let (Just bdy) = tu ^? (ix "main" . functionDefinition . body)
  void $ runBrowserT randomStrategy' bdy


randomStrategy' :: (HasTranslationUnit env, HasLogFunc env, HasDiffParameters env) => BrowserT (RIO env) ()
randomStrategy' = do
  randomStep
  vars <- findReads
  if null vars
    then randomStrategy' -- start over
    else tryout $ do
      (v,ty) <- chooseOneOf vars
      asrt <- mkAssertion v ty
      insertBefore asrt
      tu0 <- view translationUnit
      tu <- buildTranslationUnit tu0
      (res :: [VerifierRun]) <- lift $ verify tu
      logInfo $ "results: " <> display (tshow res)
  -- iterate
  randomStrategy'


chooseOneOf :: (MonadIO m) => [a] ->  m a
chooseOneOf options = do
  i <- liftIO $ getStdRandom $ randomR (0, length options - 1)
  return (options P.!! i)



-- | walks a random step in the ast
randomStep :: (MonadIO m, MonadBrowser m) => m ()
randomStep = do
  d <- chooseOneOf [Up, Down, Next, Prev]
  success <- go d
  unless success randomStep


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
           identifier = CVar (builtinIdent "__VERIFIER_assert") (undefNode,voidType)
           expression = CBinary CNeqOp var constant' (undefNode, boolType)
           stmt       = CExpr (Just $ CCall identifier [expression] (undefNode, voidType)) (undefNode, voidType)
      return stmt
