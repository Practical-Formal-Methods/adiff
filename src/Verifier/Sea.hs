module Verifier.Sea(seahorn) where

import           RIO

import           Verifier.Util

seahorn :: Verifier
seahorn = def { verifierName = "seahorn"
              , execute = runSeahorn
              }

-- | Apparently, the script "sea" has to be run from the directory it is installed in.
-- At the moment, the path /verifiers/seahorn/bin is hard coded. TODO: Use which or similar to change cwd based on that.
runSeahorn :: FilePath -> RIO VerifierEnv (VerifierResult, Timing)
runSeahorn fn = do
  let cmd = (shell $ "./sea pf "  ++ fn) {cwd = Just "/verifiers/seahorn/bin"}
  (_, out, _, timing) <- execTimed cmd ""
  debugOutput "seahorn" out
  let res = case lastMay (lines out) of
              Just "sat"   -> VerificationFailed
              Just "unsat" -> VerificationSuccessful
              _            -> VerificationResultUnknown
  return (res, timing)
