{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{- Custom prelude, to be used with all vdiff projects -}
module VDiff.Prelude
  ( module RIO
  , module VDiff.Prelude.Types
  , (^.)
  , view
  -- * small utilities
  , nowISO
  , printD
  , displayList
  -- * safe functions
  , readMay
  , headMay
  ) where

import           Control.Lens
import           Control.Lens.Operators
import           Data.List              (intersperse)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Format
import           Options.Applicative
import           RIO                    hiding (view, (^.))
import           Safe


import           VDiff.Prelude.Types


nowISO :: (MonadIO m) => m Text
nowISO =  do
  utc <- liftIO getCurrentTime
  let x = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) utc
  return $ T.pack x

printD :: (Display a, MonadIO m) => a -> m ()
printD = liftIO . T.putStrLn . utf8BuilderToText . display

displayList :: Display a => [a] -> Utf8Builder
displayList xs = mconcat $ intersperse ", " (map display xs)
