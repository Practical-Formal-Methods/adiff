module Verifier.Sea(seahorn) where

import           Data.List     (isInfixOf)
import           Verifier.Util

seahorn :: Verifier
seahorn = def { verifierName = "seahorn"
              , execute = runSeahorn
              }

runSeahorn :: FilePath -> IO (VerifierResult, Timing)
runSeahorn fn = withSpec reachSafety $ \sp -> do
  let cmd = shell $ "sea --spec " ++ sp  ++ " " ++ fn
  (_, out, timing) <- execTimed cmd ""
  let res
        | "BRUNCH_STAT Result True" `isInfixOf` out = VerificationSuccessful
        | "BRUNCH_STAT Result FALSE"  `isInfixOf` out = VerificationFailed
        | otherwise = VerificationResultUnknown
  return (res, timing)
