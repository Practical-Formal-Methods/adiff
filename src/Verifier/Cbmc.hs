module Verifier.Cbmc (cbmc) where

import Verifier.Util

-- | This is the cbmc verifier. The last line of its output on stdout tells us
-- the result of the verification.
cbmc :: Verifier
cbmc = def { verifierName = "cbmc", execute =  run, version = cbmcVersion }
  where run fn = do
          let cmd = "cbmc --32 --error-label ERROR " ++ fn
          (exitCode, out, _) <- readCreateProcessWithExitCode (shell cmd) ""
          let lastLine = last $ lines out
          case (exitCode, lastLine) of
              (ExitSuccess,"VERIFICATION FAILED")     -> return VerificationFailed
              (ExitSuccess,"VERIFICATION SUCCESSFUL") -> return VerificationSuccessful
              _                                       -> return VerificationFailed
        cbmcVersion = Just . head . lines <$> readCreateProcess (shell "cbmc --version") ""
