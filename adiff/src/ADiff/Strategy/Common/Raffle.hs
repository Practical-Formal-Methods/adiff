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

{- Often you want to choose a random element from a universe not uniformly but
   with probabilities attached to each element. This module provides a "Raffle",
   a container into which one can insert elements attached with a number that
   denotes the number of "raffle tickets". One can then draw from the raffle.
   Where the relative probability to draw an element is proportional to the
   number of tickets.

  At the moment it uses trivial implementation where a draw is $O(n)$. A faster
  implementation would use an interval tree.
-}
module ADiff.Strategy.Common.Raffle
  ( Raffle
  , countElements
  , countTickets
  , drawM
  , fromList
  , fromList1
  , insert
  , toList
  ) where

import           Control.Monad.Random hiding (fromList)
import qualified Prelude              as P
import           RIO                  hiding (toList)

data Raffle a = Raffle
  { toList        :: [(a, Double)]
  , countTickets  :: !Double
  , countElements ::  Int
  }

emptyRaffle :: Raffle a
emptyRaffle = Raffle [] 0 0

insert :: (a, Double) -> Raffle a -> Raffle a
insert (e, tickets) r@(Raffle l t n)
  | tickets > 0 = Raffle ((e, tickets) : l) (t + tickets) (n + 1)
  | otherwise = r

fromList :: [(a, Double)] -> Raffle a
fromList = foldr insert emptyRaffle

-- | giving every element exactly one ticket
fromList1 :: [a] -> Raffle a
fromList1 l = ADiff.Strategy.Common.Raffle.fromList $ zip l (P.repeat 1.0)

draw :: (RandomGen g)  => g -> Raffle a -> (a, g)
draw g (Raffle l total _) =
  let (i,g') = randomR (0, total) g
  in (getAt i total l, g')


drawM :: (MonadRandom m) => Raffle a -> m a
drawM (Raffle l total _) = do
  i <- getRandomR (0, total)
  return $! getAt i total l



getAt :: Double -> Double -> [(a,Double)] -> a
getAt i total ((x,t):xs)
  | i + t >= total = x
  | i <= 0 = x
  | otherwise = getAt (i-t) (total - t) xs
getAt _ _ [] = error "invalid state in drawM (maybe tried to draw empty list?)"
