{-# LANGUAGE MultiParamTypeClasses  #-}

module VDiff.Strategy.RandomUniform
  ( randomUniformStrategy
  , depthFirstStrategy
  , breadthFirstStrategy
  ) where

import VDiff.Prelude

import           Control.Monad.List
import           Data.Functor.Identity

import           Language.C
import           VDiff.Instrumentation
import           VDiff.Strategy.Common
import qualified VDiff.Strategy.Common.Raffle       as Raffle

type Prioritization = ExprRead -> Double


-- | Every position has priority 1.0
randomUniformStrategy :: (IsStrategyEnv env) => RIO env ()
randomUniformStrategy = mkStrategy  (const 1.0)

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
  tu <- view translationUnit
  bdg <- view (diffParameters . budget)
  sm <- view (diffParameters . searchMode)

  let (reads :: Raffle.Raffle ExprRead) = Raffle.fromList $ zipMap prioritize $ findAllReads sm tu
  let constantPool = findAllConstants tu

  unless (Raffle.countElements reads == 0) $
    replicateM_ bdg $ do
      r <- Raffle.drawM reads
      let ty = getType $ r ^. expression
      let constants = Raffle.fromList1 $ map Just $ lookupPool ty constantPool
      let constants' = Raffle.insert (Nothing, max 1 $ Raffle.countTickets constants) constants
      constant <- Raffle.drawM constants' >>= \case
        Nothing -> mkRandomConstant ty
        Just c -> return c
      let asrt = assertUnequal (r ^. expression) constant
      let tu' = insertAt (r ^. position) asrt tu
      verify tu'
  where
    zipMap f l = zip l (map f l)

insertAt :: AstPosition -> CStatement SemPhase -> CTranslationUnit SemPhase -> CTranslationUnit SemPhase
insertAt p asrt tu = snd $ runIdentity $ runBrowserT (gotoPosition p >> insertBefore asrt) tu
