module VDiff.Verifier.Cbmc (cbmc) where

import qualified Data.ByteString.Char8 as C8
import           RIO
import qualified RIO.List.Partial      as L

import           VDiff.Verifier.Util

-- | This is the cbmc verifier. The last line of its output on stdout tells us
-- the result of the verification.
cbmc :: Verifier
cbmc = def { verifierName = "cbmc"
           , execute = runCbmc
           , version = cbmcVersion
           }

runCbmc :: FilePath -> RIO VerifierEnv VerifierResult
runCbmc fn = do
  let cmd = shell $ "cbmc --32 --error-label ERROR " ++ fn
  withTiming cmd "" $ \ec out err ->
    case L.last (C8.lines out) of
         ("VERIFICATION FAILED")     -> return Sat
         ("VERIFICATION SUCCESSFUL") -> return Unsat
         l                           -> do
           logWarn $ "unexpected return of cbmc: " <> display (tshow ec) <> "last line: " <> display (tshow l)
           return Unknown

cbmcVersion :: IO (Maybe String)
cbmcVersion = headMay . lines <$> readCreateProcess (shell "cbmc --version") ""
