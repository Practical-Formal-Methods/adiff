{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Random.Shuffle.Extended
  ( System.Random.Shuffle.Extended.shuffleM
  ) where

import           Control.Monad.Random
import           System.Random.Shuffle as R


-- wrapper around shuffleM that catches the bug around []
shuffleM :: (MonadRandom m) => [a] -> m [a]
shuffleM [] = return []
shuffleM l  = R.shuffleM l


