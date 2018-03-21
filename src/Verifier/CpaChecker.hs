module Verifier.CpaChecker (cpaChecker) where

import RIO

import           Verifier.Util

import           Data.List     (isPrefixOf)
import           Data.Maybe    (listToMaybe)

cpaChecker :: Verifier
cpaChecker = def { verifierName = "cpachecker", execute = cpaExecute, version = cpaVersion}

cpaExecute :: FilePath -> RIO VerifierEnv (VerifierResult, Timing)
cpaExecute fn = withSpec reachSafety $ \spec -> do
  let cmd = shell $ "cpa.sh -default -nolog -noout -spec " ++ spec ++ " " ++ fn
  (_,out,timing) <- execTimed cmd ""
  let out' = filter ("Verification result" `isPrefixOf`) $ lines out
  let res = case out' of
        (s:_) | "Verification result: TRUE" `isPrefixOf` s -> VerificationSuccessful
              | "Verification result: FALSE" `isPrefixOf` s -> VerificationFailed
        _ -> VerificationResultUnknown
  return (res, timing)

cpaVersion :: IO (Maybe String)
cpaVersion = do
    (_,out,_) <- readCreateProcessWithExitCode (shell "cpa.sh -h") ""
    let v = listToMaybe $ filter (\l -> "CPAchecker" `isPrefixOf` l) $ lines out
    return v

