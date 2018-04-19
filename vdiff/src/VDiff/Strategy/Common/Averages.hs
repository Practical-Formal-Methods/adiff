{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | handles all things regarding the computation of moving averages

module VDiff.Strategy.Common.Averages where

import RIO
import Control.Lens
import Control.Monad.State

-- Any monad that has a state with averages and a counter can be a AverageMonad

data SimpleAverageState = SimpleAverageState
  { _averages  :: [Double]     -- ^ average runtime for each verifier
  , _averagesN :: !Double         -- ^ number of past runs
  }
makeFieldsNoPrefix ''SimpleAverageState

class ( Monad m, MonadState st m
      , HasAveragesN st Double
      , HasAverages st [Double]
      ) => AverageMonad st m

instance AverageMonad SimpleAverageState (State SimpleAverageState)
instance AverageMonad SimpleAverageState (StateT SimpleAverageState IO)

-- "cumulative moving average"
updateAverages ::  (AverageMonad st m) => [Double] -> m ()
updateAverages times = do
  n <- use averagesN
  olds <- use averages
  let averages' = [ updateAverage n new old | new <- times | old <- olds ]
  averages .= averages'
  averagesN += 1

updateAverage :: Double -> Double -> Double -> Double
updateAverage n newTime old = (n * old + newTime ) /  (n + 1.0)
