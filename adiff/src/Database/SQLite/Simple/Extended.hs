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
  liftIO $ SQL.withConnection fn $ \conn -> unliftIO env $ act conn

fold_ :: (FromRow row, MonadIO m, MonadUnliftIO m)
      => Connection -> Query -> a -> ( a -> row -> m a) -> m a
fold_ conn q z f = do
  env <- askUnliftIO
  liftIO $ SQL.fold_ conn q z (\x r -> unliftIO env (f x r))

--TODO: use dlist
foldBufferedIO_ :: (FromRow row)
      => Int -> Connection -> Query -> a -> ( a -> [row] -> IO a) -> IO a
foldBufferedIO_ bufSize conn q z f = do
  (acc, buf) <- SQL.fold_ conn q (z,[]) innerF
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
