module VDiff.Verifier
  ( allVerifiers
  , debuggingVerifiers
  , lookupVerifier
  ) where

import           VDiff.Prelude

import           VDiff.Verifier.Cbmc
import           VDiff.Verifier.CpaChecker
import           VDiff.Verifier.CProver
import           VDiff.Verifier.Debug
import           VDiff.Verifier.Klee
import           VDiff.Verifier.Sea
import           VDiff.Verifier.Ultimate
import           VDiff.Verifier.Smack
import           VDiff.Verifier.CrabLlvm

allVerifiers :: [Verifier]
allVerifiers = [ cbmc
               , cpaChecker
               , klee
               , crabLlvm
               , seacrab
               , seahorn
               , smack
               , uautomizer
               , utaipan
               -- , cprover2Ls -- does not work
               ]


lookupVerifier :: Text -> Maybe Verifier
lookupVerifier n = headMay $ filter (\v -> v ^. name == n)  allVerifiers
