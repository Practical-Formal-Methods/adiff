{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{- Custom prelude, to be used with all vdiff projects -}
module VDiff.Prelude
  ( module RIO
  , module VDiff.Prelude.Types
  , (^.)
  , view
  -- * building blocks for every application
  , runVDiffApp
  -- * small utilities
  , nowISO
  ) where

import           Control.Lens
import           Control.Lens.Operators
import qualified Data.Text                          as T
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Format
import           Options.Applicative
import           RIO                                hiding (view, (^.))


import           VDiff.Prelude.Internal.Application
import           VDiff.Prelude.Types


nowISO :: (MonadIO m) => m Text
nowISO =  do
  utc <- liftIO $ getCurrentTime
  let x = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) utc
  return $ T.pack x
