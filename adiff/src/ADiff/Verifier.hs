module ADiff.Verifier
  ( allVerifiers
  , debuggingVerifiers
  , lookupVerifier
  ) where

import           ADiff.Prelude

import           ADiff.Verifier.Cbmc
import           ADiff.Verifier.CpaChecker
import           ADiff.Verifier.CProver
import           ADiff.Verifier.Debug
import           ADiff.Verifier.Klee
import           ADiff.Verifier.Sea
import           ADiff.Verifier.Ultimate
import           ADiff.Verifier.Smack
import           ADiff.Verifier.CrabLlvm

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

