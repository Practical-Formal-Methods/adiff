module ADiff.Verifier.Smack (smack) where

import           ADiff.Prelude
import           ADiff.Verifier.Util

import           System.Process

smack = Verifier "smack" executeSmack versionSmack

executeSmack :: FilePath -> RIO VerifierEnv VerifierResult
executeSmack fp =
  withSystemTempDirectory "smack" $ \dir -> do
    let cmd = shell $ "cd " ++ dir ++ "; " ++ "CORRAL=\"mono /tmp/corral/bin/Release/corral.exe\" smack -x=svcomp --clang-options=-m32 --unroll 1000 --loop-limit 1000 "  ++ fp
    withTiming cmd "" $ \ec _ _ ->
      case ec of
        ExitFailure _ -> return Sat
        ExitSuccess -> return Unsat

versionSmack = undefined
