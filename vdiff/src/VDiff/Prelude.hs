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
  ) where

import           Control.Lens
import           Control.Lens.Operators
import           Options.Applicative
import           RIO                                hiding (view, (^.))

import           VDiff.Prelude.Internal.Application
import           VDiff.Prelude.Types

