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

data AssertionTemplate = AssertionTemplate
  { _position   :: AstPosition
  , _identifier :: Ident
  , _varType    :: Type
  , _constant   :: Maybe (CConstant SemPhase)
  } deriving (Show, Eq)

makeFieldsNoPrefix ''AssertionTemplate

randomUniformStrategy :: (IsStrategyEnv env) => RIO env ()
randomUniformStrategy = do
  logDebug "starting randomUniformStrategy"
  tu <- view translationUnit
  bdg <- view (diffParameters . budget)
  candidates <- shuffleM $ genCandidates tu (findAllConstants tu)
  unless (null candidates) $  do
    forM_ (take bdg $ P.cycle candidates) $ \template -> do
      constant <- case template ^. constant of
        Just c  -> return c
        Nothing -> mkRandomConstant (template ^. varType)
      let asrt = assertUnequal (template ^. identifier) constant
      let tu' = insertAt (template ^. position) asrt tu
      verify tu'

insertAt :: AstPosition -> CStatement SemPhase -> CTranslationUnit SemPhase -> CTranslationUnit SemPhase
insertAt p asrt tu = snd $ runIdentity $ runBrowserT (gotoPosition p >> insertBefore asrt) tu


-- | returns all
genCandidates :: CTranslationUnit SemPhase
              -> ConstantPool
              -> [AssertionTemplate]
genCandidates tu pool = do
  (p,i,ty) <- findReads tu
  fromPool <- [True, False]
  constants <- if fromPool
              then return $ Just <$> lookupPool ty pool
              else return [Nothing]
  c <- constants
  return $ AssertionTemplate p i ty c
