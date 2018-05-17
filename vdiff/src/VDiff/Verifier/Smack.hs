module VDiff.Verifier.Smack (smack) where

import           RIO
import           VDiff.Types
import           VDiff.Verifier.Util

import           System.Process

smack = Verifier
  { verifierName = "smack"
  , execute = executeSmack
  , version = versionSmack
  }

executeSmack :: FilePath -> RIO VerifierEnv VerifierResult
executeSmack fp = do
  let cmd = shell $ "CORRAL=\"mono /tmp/corral/bin/Release/corral.exe\" smack -x=svcomp "  ++ fp
  withTiming cmd "" $ \ec _ _ -> do
    case ec of
      ExitFailure _ -> return Sat
      ExitSuccess -> return Unsat

versionSmack = undefined
