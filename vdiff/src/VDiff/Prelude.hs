{- Custom prelude, to be used with all vdiff projects -}
module VDiff.Prelude
  ( module RIO
  , module VDiff.Prelude.Types
  , (^.)
  , view
  ) where

import RIO hiding ((^.), view)
import Control.Lens
import Control.Lens.Operators


import VDiff.Prelude.Types

