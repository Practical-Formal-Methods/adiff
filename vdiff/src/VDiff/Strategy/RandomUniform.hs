module VDiff.Strategy.RandomUniform (randomUniformStrategy) where

import           RIO
import           VDiff.Types

import           VDiff.Instrumentation

randomUniformStrategy :: (IsStrategyEnv env) => RIO env ()
randomUniformStrategy = do
  tu <- view translationUnit
  let pos = findReads tu
  logDebug $ display $ tshow pos
  undefined
  -- bdg <- view (diffParameters . budget)

  -- take bdg $ shuffle pos

