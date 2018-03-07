module Verifier.CpaChecker (cpaChecker) where

import           Verifier.Util

import           Data.List     (isPrefixOf)
import           Data.Maybe    (listToMaybe)

cpaChecker :: Verifier
cpaChecker = def { verifierName = "cpachecker", execute = cpaExecute, version = cpaVersion}
  where
    cpaExecute fn = withSpec reachSafety $ \spec -> do
      let cmd = shell $ "cpa.sh -default -nolog -noout -spec " ++ spec ++ " " ++ fn
      (_,out,_) <- readCreateProcessWithExitCode cmd ""
      let out' = filter ("Verification result" `isPrefixOf`) $ lines out
      case out' of
        (s:_) | "Verification result: TRUE" `isPrefixOf` s -> return VerificationSuccessful
              | "Verification result: FALSE" `isPrefixOf` s -> return VerificationFailed
        _ -> return VerificationResultUnknown

    cpaVersion = do
        (_,out,_) <- readCreateProcessWithExitCode (shell "cpa.sh -h") ""
        let v = listToMaybe $ filter (\l -> "CPAchecker" `isPrefixOf` l) $ lines out
        return v

