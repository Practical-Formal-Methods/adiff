module VDiff.Strategy
  ( executeStrategy
  , strategyName
  , availableStrategies
  ) where

import           RIO

import           VDiff.Strategy.RandomUniform
import           VDiff.Strategy.RandomWalk
import           VDiff.Strategy.Smart
import           VDiff.Types


availableStrategies =
  [ RandomWalkStrategy
  , RandomUniformStrategy
  , SmartStrategy
  , DepthFirstStrategy
  , BreadthFirstStrategy
  ]

strategyName :: Strategy -> String
strategyName RandomWalkStrategy    = "random-walk"
strategyName RandomUniformStrategy = "random-uniform"
strategyName SmartStrategy         = "smart"
strategyName DepthFirstStrategy    = "dfs"
strategyName BreadthFirstStrategy  = "bfs"


executeStrategy :: (IsStrategyEnv env) => Strategy -> RIO env ()
executeStrategy RandomWalkStrategy    = randomWalkStrategy
executeStrategy RandomUniformStrategy = randomUniformStrategy
executeStrategy SmartStrategy         = smartStrategy
executeStrategy DepthFirstStrategy    = depthFirstStrategy
executeStrategy BreadthFirstStrategy  = breadthFirstStrategy
