module Verifier.Cbmc (cbmc) where

import           Verifier.Util

-- | This is the cbmc verifier. The last line of its output on stdout tells us
-- the result of the verification.
cbmc :: Verifier
cbmc = def { verifierName = "cbmc", execute = runCbmc, version = cbmcVersion }

runCbmc :: FilePath -> IO (VerifierResult, Timing)
runCbmc fn = do
  let cmd = "cbmc --32 --error-label ERROR " ++ fn
  (exitCode, out, timing) <- execTimed (shell cmd) ""
  let lastLine = last $ lines out
      res = case (exitCode, lastLine) of
        (ExitSuccess,"VERIFICATION FAILED")     -> VerificationFailed
        (ExitSuccess,"VERIFICATION SUCCESSFUL") -> VerificationSuccessful
        _                                       -> VerificationFailed
  return (res, timing)


cbmcVersion :: IO (Maybe String)
cbmcVersion = Just . head . lines <$> readCreateProcess (shell "cbmc --version") ""
