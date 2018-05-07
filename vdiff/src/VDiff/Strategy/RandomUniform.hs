{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module VDiff.Strategy.RandomUniform
  ( randomUniformStrategy
  , depthFirstStrategy
  , breadthFirstStrategy
  ) where

import           Control.Monad.List
import           Data.Functor.Identity
import           RIO
import           System.Random.Shuffle.Extended()

import           Language.C
import           VDiff.Instrumentation
import           VDiff.Strategy.Common
import qualified VDiff.Strategy.Common.Raffle       as Raffle
import           VDiff.Types

type Prioritization  = VarRead -> Double


-- | Every position has priority 1.0
randomUniformStrategy :: (IsStrategyEnv env) => RIO env ()
randomUniformStrategy = mkStrategy  (\_ -> 1.0)

-- | Priority proportional to depth
depthFirstStrategy :: (IsStrategyEnv env) => RIO env ()
depthFirstStrategy = mkStrategy p
  where
    p r = fromIntegral $ astDepth (r ^. position)

-- | Priority anti-proportional to depth
breadthFirstStrategy :: (IsStrategyEnv env) => RIO env ()
breadthFirstStrategy = mkStrategy p
  where
    p r = 1.0 / fromIntegral (astDepth (r ^. position))





mkStrategy :: (IsStrategyEnv env) => Prioritization -> RIO env ()
mkStrategy prioritize = do
  logDebug "starting randomUniformStrategy"
  tu <- view translationUnit
  bdg <- view (diffParameters . budget)

  let positions = Raffle.fromList $ zipMap prioritize $ findAllReads tu
  let constantPool = findAllConstants tu

  unless (Raffle.countElements positions == 0) $ do
    replicateM_ bdg $ do
      r <- Raffle.drawM positions
      let constants = Raffle.fromList1 $ map Just $ lookupPool (r ^. varType) constantPool
      let constants' = Raffle.insert (Nothing, Raffle.countTickets constants) constants
      constant <- Raffle.drawM constants' >>= \case
        Nothing -> mkRandomConstant (r ^. varType)
        Just c -> return c
      let asrt = assertUnequal (r ^. identifier) constant
      let tu' = insertAt (r ^. position) asrt tu
      verify tu'
  where
    zipMap f l = zip l (map f l)

insertAt :: AstPosition -> CStatement SemPhase -> CTranslationUnit SemPhase -> CTranslationUnit SemPhase
insertAt p asrt tu = snd $ runIdentity $ runBrowserT (gotoPosition p >> insertBefore asrt) tu
