{-# LANGUAGE OverloadedStrings #-}

module VDiff.Verifier.CrabLlvm where


import qualified Data.ByteString.Char8 as C8
import           Data.Either
import qualified Data.Text             as T
import qualified Prelude               as P
import qualified RIO.List              as L
import           Text.Regex.Posix
import           VDiff.Prelude

import           VDiff.Verifier.Util

crabLlvm :: Verifier
crabLlvm = Verifier "crab-llvm" executeCrabLlvm crabVersion

-- | Apparently, the script "sea" has to be run from the directory it is installed in.
-- At the moment, the path /verifiers/crab-llvm/bin is hard coded. TODO: Use which or similar to change cwd based on that.
executeCrabLlvm :: FilePath -> RIO VerifierEnv VerifierResult
executeCrabLlvm fn = do
  flgs <- map T.unpack <$> view extraFlags
  let cmd = (shell $ "./crabllvm.py -m 32 --crab-inter --crab-check=assert " ++ L.intercalate " " flgs ++ " "  ++ fn) {cwd = Just "/verifiers/crab-llvm/bin"}
  withTiming cmd "" $ \_ out _ -> do
    let (x,wrn,err) = extractNumbers out
    if wrn == 0 && err == 0
      then return Unsat
      else return Sat

crabVersion :: IO (Maybe String)
crabVersion = return Nothing

extractNumbers :: ByteString -> (Int, Int, Int)
extractNumbers out =
    let [ln_safe, ln_error, ln_total] = take 3 . drop 1 . dropWhile (\l -> not (l =~ ("^(\\*)+ ANALYSIS RESULTS .*" :: ByteString))) . C8.lines $ out
        [safe, warning, error]  = P.read . C8.unpack . P.head . C8.words <$> [ln_safe, ln_error, ln_total]
    in (safe,warning,error)

