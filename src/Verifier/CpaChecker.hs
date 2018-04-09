module Verifier.CpaChecker (cpaChecker) where

import           RIO

import           Verifier.Util

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.List             (isPrefixOf)
import           Data.Maybe            (listToMaybe)

cpaChecker :: Verifier
cpaChecker = def { verifierName = "cpachecker", execute = cpaExecute, version = cpaVersion}

cpaExecute :: FilePath -> RIO VerifierEnv VerifierResult
cpaExecute fn = withSpec reachSafety $ \spec -> do
  let cmd = shell $ "cpa.sh -default -nolog -noout -spec " ++ spec ++ " " ++ fn
  withTiming cmd "" $ \_ out _ -> do
      let out' = filter ("Verification result" `BS.isPrefixOf`) $ C8.lines out
      let verdict = case out' of
            (s:_) | "Verification result: TRUE" `BS.isPrefixOf` s -> Unsat
                  | "Verification result: FALSE" `BS.isPrefixOf` s -> Sat
            _ -> Unknown
      return verdict


cpaVersion :: IO (Maybe String)
cpaVersion = do
    out <- readCreateProcess (shell "cpa.sh -h") ""
    let v = listToMaybe $ filter (\l -> "CPAchecker" `isPrefixOf` l) $ lines out
    return v

