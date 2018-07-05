module VDiff.Verifier.Sea(seahorn, seacrab) where

import           VDiff.Prelude

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text             as T
import qualified RIO.List              as L

import           VDiff.Verifier.Util

seahorn :: Verifier
seahorn = Verifier "seahorn" (runSea False) seaVersion

seacrab :: Verifier
seacrab = Verifier "seacrab" (runSea True) seaVersion

-- | Apparently, the script "sea" has to be run from the directory it is installed in.
-- At the moment, the path /verifiers/seahorn/bin is hard coded. TODO: Use which or similar to change cwd based on that.
runSea :: Bool -> FilePath -> RIO VerifierEnv VerifierResult
runSea enableCrab fn = do
  flgs <- map T.unpack <$> view extraFlags
  let crabFlag = if enableCrab then "--crab" else ""
      cmd = (shell $ "./sea " ++ crabFlag ++ " " ++ L.intercalate " " flgs ++ " pf "  ++ fn) {cwd = Just "/verifiers/seahorn/bin"}
  withTiming cmd "" $ \_ out _ ->
    case lastMay (C8.lines out) of
      Just "sat"   -> return Sat
      Just "unsat" -> return Unsat
      _            -> return Unknown

seaVersion :: IO (Maybe String)
seaVersion = return Nothing
