{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Implements a very simple strategy. Currently only instruments the main function
module VDiff.Strategy.RandomWalk (randomWalkStrategy) where

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

instance MonadRandom (RandomS env) where
  getRandomR r  = liftIO $ getRandomR r
  getRandomRs r = liftIO $ getRandomRs r
  getRandom     = liftIO getRandom
  getRandoms    = liftIO getRandoms

runRandomS :: IsStrategyEnv env => RandomState -> CTranslationUnit SemPhase -> RIO env (((), RandomState), CTranslationUnit SemPhase)
runRandomS initState = runBrowserT (runStateT (unRandom randomWalkStrategy') initState)


randomWalkStrategy :: (IsStrategyEnv env) => RIO env ()
randomWalkStrategy = do
  tu <- view translationUnit
  b <- view (diffParameters . budget)
  let initState = RandomState b 0
  void $ runRandomS initState tu


randomWalkStrategy' :: (IsStrategyEnv env) => RandomS env ()
randomWalkStrategy' = do
  bdg <- use budget
  stps <- use stepsWithoutInsertion
  when (bdg > 0 && stps < 1000) $ do
    randomStep
    stepsWithoutInsertion += 1
    vars <- currentReads
    unless (null vars) $ tryout $ do
        (Just e) <- chooseOneOf vars
        asrt <- mkRandomAssertion e
        stepsWithoutInsertion .= 0
        insertBefore asrt
        tu <- buildTranslationUnit
        budget -= 1
        (_, conclusion) <- verify tu
        logInfo $ "conclusion : " <> display (tshow conclusion)
    -- iterate
    randomWalkStrategy'



-- | walks a random step in the ast
randomStep :: (MonadRandom m, MonadBrowser m) => m ()
randomStep = do
  findCalledFunction >>= \case
          Nothing -> oneStep
          Just fn -> randomlyBranch [gotoFunction fn, oneStep]
  where
    oneStep = do
      (Just d) <- chooseOneOf [Up, Down, Next, Prev]
      success <- go d
      unless success oneStep
