module VDiff.Verifier.Smack (smack) where

import           VDiff.Prelude
import           VDiff.Verifier.Util

import           System.Process

smack = Verifier
  { verifierName = "smack"
  , execute = executeSmack
  , version = versionSmack
  }

executeSmack :: FilePath -> RIO VerifierEnv VerifierResult
executeSmack fp = do
  withSystemTempDirectory "smack" $ \dir -> do
    let cmd = shell $ "cd " ++ dir ++ "; " ++ "CORRAL=\"mono /tmp/corral/bin/Release/corral.exe\" smack -x=svcomp --clang-options=-m32"  ++ fp
    withTiming cmd "" $ \ec _ _ -> do
      case ec of
        ExitFailure _ -> return Sat
        ExitSuccess -> return Unsat

versionSmack = undefined
