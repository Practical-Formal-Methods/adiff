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
