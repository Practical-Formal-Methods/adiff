module VDiff.Verifier.Sea(seahorn, seacrab) where

import           RIO

import qualified Data.ByteString.Char8 as C8

import           VDiff.Verifier.Util

seahorn :: Verifier
seahorn = Verifier
  { verifierName = "seahorn"
  , execute = runSea False
  , version = seaVersion
  }

seacrab :: Verifier
seacrab = Verifier
  { verifierName = "seacrab"
  , execute = runSea True
  , version = seaVersion
  }

-- | Apparently, the script "sea" has to be run from the directory it is installed in.
-- At the moment, the path /verifiers/seahorn/bin is hard coded. TODO: Use which or similar to change cwd based on that.
runSea :: Bool -> FilePath -> RIO VerifierEnv VerifierResult
runSea crab fn = do
  let flags = if crab then "--crab" else ""
      cmd = (shell $ "./sea " ++ flags ++ " pf "  ++ fn) {cwd = Just "/verifiers/seahorn/bin"}
  withTiming cmd "" $ \_ out _ ->
    case lastMay (C8.lines out) of
      Just "sat"   -> return Sat
      Just "unsat" -> return Unsat
      _            -> return Unknown

seaVersion :: IO (Maybe String)
seaVersion = return Nothing
