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

{-# LANGUAGE ScopedTypeVariables #-}

module ADiff.Util.ResourcePool where

import           Control.Concurrent.STM.TQueue
import           UnliftIO.Concurrent
import           ADiff.Prelude

newtype ResourcePool r = ResourcePool { unPool :: TQueue r }


newResourcePool :: (MonadIO m) => [r] -> m (ResourcePool r)
newResourcePool rs = atomically $ do
  (tq :: TQueue r) <- newTQueue
  mapM_ (writeTQueue tq) rs
  return (ResourcePool tq)

withResourcePool :: (MonadUnliftIO m) => ResourcePool r -> [r -> m a] -> m [a]
withResourcePool p actions = do
  mvars <- forM actions $ \a -> do
    m <- newEmptyMVar
    forkIO $ do
      -- take a resource
      r <- atomically $ readTQueue (unPool p)
      x <- a r `finally` atomically (writeTQueue (unPool p) r)
      -- write to mvar
      putMVar m x
    return m
  -- wait for all mvars
  mapM takeMVar mvars
