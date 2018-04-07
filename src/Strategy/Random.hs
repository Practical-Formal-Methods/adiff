{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Strategy.Random (randomStrategy) where

import qualified Prelude              as P
import           RIO

import           Control.Lens         hiding (view)
import           Control.Monad.State
import           Language.C.Data.Lens
import           System.Random

import           Instrumentation
import           Types

import           Strategy.Util

newtype RandomState = RandomState { _budget :: Int }
makeFieldsNoPrefix ''RandomState


newtype RandomS env a = RandomS
  { unRandom :: StateT RandomState (BrowserT (RIO env)) a
  } deriving (Functor, Applicative, Monad, MonadBrowser, MonadIO, MonadReader env, MonadState RandomState)

runRandomS :: IsStrategyEnv env => RandomState -> Stmt -> RIO env (((), RandomState), Stmt)
runRandomS initState = runBrowserT (runStateT (unRandom randomStrategy') initState)


randomStrategy :: (IsStrategyEnv env) => RIO env ()
randomStrategy = do
  tu <- view translationUnit
  let (Just bdy) = tu ^? (ix "main" . functionDefinition . body)
  b <- view (diffParameters . budget)
  let initState = RandomState b
  void $ runRandomS initState bdy


randomStrategy' :: (IsStrategyEnv env) => RandomS env ()
randomStrategy' =
  whenM ((>0) <$> use budget) $ do
    randomStep
    vars <- findReads
    unless (null vars) $ tryout $ do
        (v,ty) <- chooseOneOf vars
        asrt <- mkAssertion v ty
        insertBefore asrt
        tu <- buildTranslationUnit
        (_, conclusion) <- verify tu
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


