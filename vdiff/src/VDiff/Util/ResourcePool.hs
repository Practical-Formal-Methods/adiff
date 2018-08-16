{-# LANGUAGE ScopedTypeVariables #-}

module VDiff.Util.ResourcePool
  ( ResourcePool
  , newResourcePool
  , execWithResourcePool
  ) where

import           Control.Concurrent.STM.TQueue
import           RIO
import           UnliftIO.Concurrent

newtype ResourcePool r = ResourcePool { unPool :: TQueue r }


newResourcePool :: (MonadIO m) => [r] -> m (ResourcePool r)
newResourcePool rs = atomically $ do
  (tq :: TQueue r) <- newTQueue
  mapM_ (writeTQueue tq) rs
  return (ResourcePool tq)

withResourcePoolPar :: (MonadUnliftIO m) => ResourcePool r -> [r -> m a] -> m [a]
withResourcePoolPar p actions = do
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


execWithResourcePool :: (MonadUnliftIO m) => ResourcePool r -> (r -> m a) -> m a
execWithResourcePool p action = do
  m <- newEmptyMVar
  forkIO $ do
    -- take a resource
    r <- atomically $ readTQueue (unPool p)
    x <- action r `finally` atomically (writeTQueue (unPool p) r)
    -- write to mvar
    putMVar m x
  takeMVar m


