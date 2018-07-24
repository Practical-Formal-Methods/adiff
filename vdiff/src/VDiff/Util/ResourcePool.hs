{-# LANGUAGE ScopedTypeVariables #-}

module VDiff.Util.ResourcePool where

import           Control.Concurrent.STM.TQueue
import           UnliftIO.Concurrent
import           VDiff.Prelude

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
