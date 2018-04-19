module VDiff.Verifier
  ( allVerifiers
  , debuggingVerifiers
  ) where

import           Data.List                 (sort)

import           VDiff.Types
import           VDiff.Verifier.Cbmc
import           VDiff.Verifier.CpaChecker
import           VDiff.Verifier.Debug
import           VDiff.Verifier.Klee
import           VDiff.Verifier.Sea
import           VDiff.Verifier.Ultimate

allVerifiers :: [Verifier]
allVerifiers = [ cbmc
               , cpaChecker
               , klee
               , seacrab
               , seahorn
               , uautomizer
               , utaipan
               ]


