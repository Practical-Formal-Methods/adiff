-- MIT License
--
-- Copyright (c) 2018 Christian Klinger
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Implements a very simple strategy. Currently only instruments the main function
module ADiff.Strategy.RandomWalk (randomWalkStrategy) where

import           ADiff.Prelude

import           Control.Lens               hiding (view)
import           Control.Monad.State.Strict
import           Language.C

import           ADiff.Instrumentation
import           ADiff.Strategy.Common

data RandomState = RandomState
  { _iteration :: !Int
  , _stepsWithoutInsertion :: !Int
  , _pool :: ConstantPool
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

runRandomS :: IsStrategyEnv env => Int -> RandomState -> CTranslationUnit SemPhase -> RIO env (((), RandomState), CTranslationUnit SemPhase)
runRandomS bdg initState = runBrowserT (runStateT (unRandom (randomWalkStrategy' bdg)) initState)


randomWalkStrategy :: (IsStrategyEnv env) => RIO env ()
randomWalkStrategy = do
  tu <- view translationUnit
  b <- view initialBudget
  let initState = RandomState 0 0 (findAllConstants tu)
  void $ runRandomS b initState tu


randomWalkStrategy' :: (IsStrategyEnv env) => Int -> RandomS env ()
randomWalkStrategy' bdg = do
  n <- use iteration
  stps <- use stepsWithoutInsertion
  pool <- use pool
  when (n < bdg && stps < 1000) $ do
    randomStep
    stepsWithoutInsertion += 1
    vars <- currentReads
    unless (null vars) $ tryout $ do
        (Just e) <- chooseOneOf vars
        (Just asrt) <- randomlyBranchMay [ mkAssertionFromPool e pool
                                         , Just <$> mkRandomAssertion e]
        stepsWithoutInsertion .= 0
        insertBefore asrt
        tu <- buildTranslationUnit
        (_, conclusion) <- verify n tu
        iteration += 1
        logInfo $ "conclusion : " <> display (tshow conclusion)
    -- iterate
    randomWalkStrategy' bdg




-- | walks a random step in the ast
randomStep :: (MonadRandom m, MonadBrowser m) => m ()
randomStep =
  findCalledFunction >>= \case
          Nothing -> oneStep
          Just fn -> void $ randomlyBranchTrue [gotoFunction fn, oneStep >> return True]
  where
    oneStep = do
      (Just d) <- chooseOneOf [Up, Down, Next, Prev]
      success <- go d
      unless success oneStep
