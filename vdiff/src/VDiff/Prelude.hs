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
  , mkHashS
  , mkHashT
  ) where

import           Control.Lens
import           Control.Lens.Operators
import           Options.Applicative
import           RIO                                hiding (view, (^.))

import           VDiff.Prelude.Internal.Application
import           VDiff.Prelude.Types                hiding (program)

import qualified Crypto.Hash.SHA1                   as SHA1
import qualified Data.ByteString.Base16             as Hex
import qualified Data.ByteString.Char8              as C8
import qualified Data.Text.Encoding                 as T

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

mkHashS :: String -> Text
mkHashS =  T.decodeUtf8 . Hex.encode . SHA1.hash . C8.pack

mkHashT :: Text -> Text
mkHashT =  T.decodeUtf8 . Hex.encode . SHA1.hash . T.encodeUtf8

