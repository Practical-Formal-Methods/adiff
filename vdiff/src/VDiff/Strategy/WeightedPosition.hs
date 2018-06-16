{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module VDiff.Strategy.WeightedPosition
  ( randomUniformStrategy
  , depthFirstStrategy
  , breadthFirstStrategy
  , randomUniformBatchStrategy
  ) where

import           VDiff.Prelude

import           Control.Monad
import           Data.Functor.Identity
import qualified Data.Vector                  as V
import           Language.C
import           VDiff.Data
import           VDiff.Instrumentation
import           VDiff.Strategy.Common
import           VDiff.Strategy.Common.Budget
import qualified VDiff.Strategy.Common.Raffle as Raffle

type Prioritization = ExprRead -> Double


-- | Every position has priority 1.0
randomUniformStrategy :: (IsStrategyEnv env) => RIO env ()
randomUniformStrategy = mkStrategy  (const 1.0)

randomUniformBatchStrategy :: (IsStrategyEnv env) => RIO env ()
randomUniformBatchStrategy = mkStrategy (const 1.0)

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
  tu              <- view translationUnit
  bdg             <- view (diffParameters . budget)
  sm              <- view (diffParameters . searchMode)
  conjunctionSize <- view (diffParameters . batchSize)

  let (reads :: Raffle.Raffle ExprRead) = Raffle.fromList $ zipMap prioritize $ findAllReads sm tu
  let constantPool = findAllConstants tu

  unless (Raffle.countElements reads == 0) $
    void $ runBudgetT bdg $ forever $ do
      r <- Raffle.drawM reads
      let ty = getType $ r ^. expression
      let constantRaffle = let tmp = Raffle.fromList1 $ map Just $ lookupPool ty constantPool
                           in Raffle.insert (Nothing, max 1 $ Raffle.countTickets tmp) tmp
      constants <- replicateM conjunctionSize $ Raffle.drawM constantRaffle
      conjuncts <- forM constants $ \case
        Nothing -> mkRandomConstant ty
        Just c -> return c
      let compoundAssertion = assertUnequals (r ^. expression) conjuncts
      let tu' = insertAt (r ^. position) compoundAssertion tu
      (_,c) <- verifyB tu'
      when (isDisagreement c) $ void $ do
        binaryIntervalSearch (V.fromList conjuncts) $ \cs -> do
          let compoundAssertion = assertUnequals (r ^. expression) (V.toList cs)
          let tu' = insertAt (r ^. position) compoundAssertion tu
          (_,c) <- verifyB tu'
          return (isDisagreement c)
  where
    zipMap f l = zip l (map f l)



-- | finds the singleton for which the test fails.
-- | A test t should have the following property: t({x}) -> t({x} u X)
binaryIntervalSearch :: (Monad m) => Vector a -> (Vector a -> m Bool) -> m (Maybe a)
binaryIntervalSearch v test
  | length v == 0 = return Nothing
  | length v == 1 = return $ Just (V.head v)
  | otherwise = do
      let pivot = V.length v `div` 2
          (v1,v2) = V.splitAt pivot v
      left <- test v1
      if left
        then binaryIntervalSearch v1 test
        else do
          right <- test v2
          if right
            then binaryIntervalSearch v2 test
            else return Nothing

insertAt :: AstPosition -> CStatement SemPhase -> CTranslationUnit SemPhase -> CTranslationUnit SemPhase
insertAt p asrt tu = snd $ runIdentity $ runBrowserT (gotoPosition p >> insertBefore asrt) tu
