{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ParallelListComp       #-}

-- | handles all things regarding the computation of moving averages

module ADiff.Strategy.Common.Averages
  ( Averages
  , updateAverages
  , getAverages
  , emptyAverages
  , getAveragesCounter
  ) where

import           ADiff.Prelude

data Averages = Averages
  { getAverages        :: ![Double]     -- ^ average runtime for each verifier
  , getAveragesCounter :: !Int -- ^ number of past runs
  } deriving (Show)

instance Display Averages where
  display (Averages av _) = displayList av


emptyAverages :: Int -> Averages
emptyAverages n = Averages (replicate n 0) 0

-- "cumulative moving average"
updateAverages ::  [Double] -> Averages -> Averages
updateAverages vals (Averages prevs n) =
  let averages' = [ updateAverage n new old | new <- vals | old <- prevs ]
  in Averages averages' (n + 1)

updateAverage ::  Int -> Double -> Double -> Double
updateAverage n newTime old = (fromIntegral n * old + newTime ) /  fromIntegral (n + 1)
