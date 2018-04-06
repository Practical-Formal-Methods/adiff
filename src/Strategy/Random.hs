{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Strategy.Random (randomStrategy) where

import qualified Prelude                as P
import           RIO

import           Control.Lens.Operators
import           Language.C.Data.Lens
import           System.Random

import           Instrumentation
import           Types

import           Strategy.Util

-- does not terminate
randomStrategy :: (IsStrategyEnv env) => RIO env ()
randomStrategy = do
  tu <- view translationUnit
  let (Just bdy) = tu ^? (ix "main" . functionDefinition . body)
  void $ runBrowserT randomStrategy' bdy


randomStrategy' :: (IsStrategyEnv env) => BrowserT (RIO env) ()
randomStrategy' = do
  randomStep
  vars <- findReads
  unless (null vars) $ tryout $ do
      (v,ty) <- chooseOneOf vars
      asrt <- mkAssertion v ty
      insertBefore asrt
      tu <- buildTranslationUnit
      res <- lift $ verify tu
      let conclusion = conclude res
      logInfo $ "conclusion : " <> display (tshow conclusion)
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


