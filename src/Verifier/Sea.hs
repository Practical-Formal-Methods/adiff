module Verifier.Sea(seahorn) where

import           RIO

import qualified Data.ByteString.Char8 as C8
import           Verifier.Util

seahorn :: Verifier
seahorn = def { verifierName = "seahorn"
              , execute = runSeahorn
              }

-- | Apparently, the script "sea" has to be run from the directory it is installed in.
-- At the moment, the path /verifiers/seahorn/bin is hard coded. TODO: Use which or similar to change cwd based on that.
runSeahorn :: FilePath -> RIO VerifierEnv VerifierResult
runSeahorn fn = do
  let cmd = (shell $ "./sea pf "  ++ fn) {cwd = Just "/verifiers/seahorn/bin"}
  (termination, out, _) <- execTimed cmd ""
  case termination of
    Nothing -> return VerifierTimedOut
    (Just (ec, timing)) -> case lastMay (C8.lines out) of
              Just "sat"   -> return $ VerifierTerminated Sat  timing
              Just "unsat" -> return $ VerifierTerminated Unsat timing
              _            -> return $ VerifierTerminated Unknown timing
