{-# LANGUAGE ScopedTypeVariables #-}

module Database.SQLite.Simple.Extended
 ( module Database.SQLite.Simple
 , withConnection
 , fold_
 , foldBuffered_
 ) where

import           Database.SQLite.Simple hiding (fold_, withConnection)
import qualified Database.SQLite.Simple as SQL

import           RIO

withConnection :: (MonadIO m, MonadUnliftIO m) => String -> (Connection -> m a) -> m a
withConnection fn act = do
  env <- askUnliftIO
  liftIO $ SQL.withConnection fn $ \conn ->  do
    unliftIO env $ act conn

fold_ :: (FromRow row, MonadIO m, MonadUnliftIO m)
      => Connection -> Query -> a -> ( a -> row -> m a) -> m a
fold_ conn q z f = do
  env <- askUnliftIO
  liftIO $ SQL.fold_ conn q z (\x r -> unliftIO env (f x r))

--TODO: use dlist
foldBufferedIO_ :: (FromRow row)
      => Int -> Connection -> Query -> a -> ( a -> [row] -> IO a) -> IO a
foldBufferedIO_ bufSize conn q z f = do
  (acc, buf) <- SQL.fold_ conn q (z,[]) (innerF)
   -- empty the buffer
  f acc buf
  where
    innerF (acc,buf) row
      | length buf < bufSize = return (acc, row:buf)
      | otherwise = do
          acc' <- f acc buf
          return (acc, [])

foldBuffered_ :: (FromRow row, MonadUnliftIO m, MonadIO m)
      => Int -> Connection -> Query -> a -> ( a -> [row] -> m a) -> m a
foldBuffered_ bufSize conn q z f = do
  env <- askUnliftIO
  liftIO $ foldBufferedIO_ bufSize conn q z (\acc rows -> unliftIO env (f acc rows))
