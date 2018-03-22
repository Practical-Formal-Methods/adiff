module Verifier.Cbmc (cbmc) where

import           RIO
import qualified RIO.List      as L

import           Verifier.Util

-- | This is the cbmc verifier. The last line of its output on stdout tells us
-- the result of the verification.
cbmc :: Verifier
cbmc = def { verifierName = "cbmc"
           , execute = runCbmc
           , version = cbmcVersion
           }

runCbmc :: FilePath -> RIO VerifierEnv (VerifierResult, Timing)
runCbmc fn = do
  let cmd = "cbmc --32 --error-label ERROR " ++ fn
  (exitCode, out, _, timing) <- execTimed (shell cmd) ""
  debugOutput "cbmc" out
  let lastLine = L.last $ lines out
      res = case (exitCode, lastLine) of
        (ExitSuccess,"VERIFICATION FAILED")     -> VerificationFailed
        (ExitSuccess,"VERIFICATION SUCCESSFUL") -> VerificationSuccessful
        _                                       -> VerificationFailed
  return (res, timing)


cbmcVersion :: IO (Maybe String)
cbmcVersion = Just . L.head . lines <$> readCreateProcess (shell "cbmc --version") ""
