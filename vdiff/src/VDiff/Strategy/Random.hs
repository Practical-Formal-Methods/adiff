{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module VDiff.Strategy.Random (randomStrategy) where

import           RIO

import           Control.Lens               hiding (view)
import           Control.Monad.State.Strict
import           Language.C

import           VDiff.Instrumentation
import           VDiff.Strategy.Common
import           VDiff.Types

data RandomState = RandomState
  { _budget                :: !Int
  , _stepsWithoutInsertion :: !Int
  }
makeFieldsNoPrefix ''RandomState


newtype RandomS env a = RandomS
  { unRandom :: StateT RandomState (BrowserT (RIO env)) a
  } deriving (Functor, Applicative, Monad, MonadBrowser, MonadIO, MonadReader env, MonadState RandomState)

runRandomS :: IsStrategyEnv env => RandomState -> CTranslationUnit SemPhase -> RIO env (((), RandomState), CTranslationUnit SemPhase)
runRandomS initState = runBrowserT (runStateT (unRandom randomStrategy') initState)


randomStrategy :: (IsStrategyEnv env) => RIO env ()
randomStrategy = do
  tu <- view translationUnit
  b <- view (diffParameters . budget)
  let initState = RandomState b 0
  void $ runRandomS initState tu


randomStrategy' :: (IsStrategyEnv env) => RandomS env ()
randomStrategy' = do
  bdg <- use budget
  stps <- use stepsWithoutInsertion
  when (bdg > 0 && stps < 1000) $ do
    randomStep
    stepsWithoutInsertion += 1
    vars <- findReads
    unless (null vars) $ tryout $ do
        (Just (v,ty)) <- chooseOneOf vars
        asrt <- mkAssertion v ty
        stepsWithoutInsertion .= 0
        insertBefore asrt
        tu <- buildTranslationUnit
        (_, conclusion) <- verify tu
        logInfo $ "conclusion : " <> display (tshow conclusion)
    -- iterate
    randomStrategy'




-- | walks a random step in the ast
randomStep :: (MonadIO m, MonadBrowser m) => m ()
randomStep = do
  (Just d) <- chooseOneOf [Up, Down, Next, Prev]
  success <- go d
  unless success randomStep


