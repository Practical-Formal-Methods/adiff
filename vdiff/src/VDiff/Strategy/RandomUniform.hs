{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module VDiff.Strategy.RandomUniform (randomUniformStrategy) where

import           Control.Lens.TH
import           Control.Monad.List
import           Data.Functor.Identity
import qualified Prelude                            as P
import           RIO
import           System.Random.Shuffle.Extended

import           Language.C
import           VDiff.Instrumentation
import           VDiff.Strategy.Common
import           VDiff.Strategy.Common.ConstantPool
import           VDiff.Types


randomUniformStrategy :: (IsStrategyEnv env) => RIO env ()
randomUniformStrategy = do
  logDebug "starting randomUniformStrategy"
  tu <- view translationUnit
  bdg <- view (diffParameters . budget)
  candidates <- shuffleM $ genCandidates tu (findAllConstants tu)
  unless (null candidates) $  do
    forM_ (take bdg $ P.cycle candidates) $ \(r,c)-> do
      constant <- case c of
        Just c  -> return c
        Nothing -> mkRandomConstant (r ^. varType)
      let asrt = assertUnequal (r ^. identifier) constant
      let tu' = insertAt (r ^. position) asrt tu
      verify tu'

insertAt :: AstPosition -> CStatement SemPhase -> CTranslationUnit SemPhase -> CTranslationUnit SemPhase
insertAt p asrt tu = snd $ runIdentity $ runBrowserT (gotoPosition p >> insertBefore asrt) tu


-- | returns all
genCandidates :: CTranslationUnit SemPhase
              -> ConstantPool
              -> [(VarRead, Maybe (CConstant SemPhase))]
genCandidates tu pool = do
  (p,i,ty) <- findAllReads tu
  fromPool <- [True, False]
  constants <- if fromPool
              then return $ Just <$> lookupPool ty pool
              else return [Nothing]
  c <- constants
  return $ ((VarRead p i ty), c)
