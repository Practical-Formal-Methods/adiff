module Verifier
  ( allVerifiers
  , debuggingVerifiers
  ) where

import           Data.List           (sort)

import           Types

import           Verifier.Cbmc
import           Verifier.CpaChecker
import           Verifier.Klee
import           Verifier.Sea
import           Verifier.Ultimate

import           Verifier.Debug

allVerifiers :: [Verifier]
allVerifiers = [ cbmc
               , cpaChecker
               , klee
               , seacrab
               , seahorn
               , uautomizer
               , utaipan
               ]


