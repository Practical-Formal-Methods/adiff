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
