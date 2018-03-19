module Verifier (allVerifiers) where

import           Data.List           (sort)

import           Types

import           Verifier.Cbmc
import           Verifier.CpaChecker
import           Verifier.Klee
import           Verifier.Sea
import           Verifier.Ultimate
import           Verifier.Vim

allVerifiers :: [Verifier]
allVerifiers = sort [cbmc, cpaChecker, klee, utaipan, uautomizer, vim, seahorn]


