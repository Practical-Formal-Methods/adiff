{-# LANGUAGE LambdaCase #-}
module ADiff.Strategy
  ( executeStrategy
  , strategyName
  , availableStrategies
  , lookupStrategy
  ) where

import           ADiff.Prelude

import           ADiff.Strategy.RandomWalk
import           ADiff.Strategy.Smart
import           ADiff.Strategy.WeightedPosition


availableStrategies :: [Strategy]
availableStrategies =
  [ RandomWalkStrategy
  , RandomUniformStrategy
  , RandomUniformBatchStrategy
  , SmartStrategy
  , DepthFirstStrategy
  , BreadthFirstStrategy
  ]

strategyName :: Strategy -> String
strategyName RandomWalkStrategy         = "random-walk"
strategyName RandomUniformStrategy      = "random-uniform"
strategyName RandomUniformBatchStrategy = "random-uniform-batch"
strategyName SmartStrategy              = "smart"
strategyName DepthFirstStrategy         = "dfs"
strategyName BreadthFirstStrategy       = "bfs"


lookupStrategy :: String -> Maybe Strategy
lookupStrategy = \case
  "random-walk"          -> Just RandomWalkStrategy
  "random-uniform"       -> Just RandomUniformStrategy
  "random-uniform-batch" -> Just RandomUniformBatchStrategy
  "smart"                -> Just SmartStrategy
  "bfs"                  -> Just BreadthFirstStrategy
  "dfs"                  -> Just DepthFirstStrategy
  _                      -> Nothing

executeStrategy :: (IsStrategyEnv env) => Strategy -> RIO env ()
executeStrategy RandomWalkStrategy         = randomWalkStrategy
executeStrategy RandomUniformStrategy      = randomUniformStrategy
executeStrategy SmartStrategy              = smartStrategy
executeStrategy DepthFirstStrategy         = depthFirstStrategy
executeStrategy BreadthFirstStrategy       = breadthFirstStrategy
executeStrategy RandomUniformBatchStrategy = randomUniformBatchStrategy
