module Verifier.Sea(seahorn) where

import           RIO
import           qualified RIO.List as L

import           Verifier.Util

seahorn :: Verifier
seahorn = def { verifierName = "seahorn"
              , execute = runSeahorn
              }

-- | Apparently, the script "sea" has to be run from the directory it is installed in.
-- At the moment, the path /verifiers/seahorn/bin is hard coded. TODO: Use which or similar to change cwd based on that.
runSeahorn :: FilePath -> IO (VerifierResult, Timing)
runSeahorn fn = do
  let cmd = (shell $ "./sea pf "  ++ fn) {cwd = Just "/verifiers/seahorn/bin"}
  (_, out, timing) <- execTimed cmd ""
  let res = case L.last (lines out) of
              "sat"   -> VerificationFailed
              "unsat" -> VerificationSuccessful
              _       -> VerificationResultUnknown
  return (res, timing)
