{-# LANGUAGE MultiWayIf #-}
module VDiff.Verifier.CProver
 ( cprover2Ls
 ) where

import qualified Data.ByteString.Char8 as C8
import           RIO
import           Safe
import           VDiff.Verifier.Util

cprover2Ls :: Verifier
cprover2Ls = Verifier "2ls" execute2Ls version2Ls

execute2Ls :: FilePath -> RIO VerifierEnv VerifierResult
execute2Ls fn = do
  let cmd = shell $ "2ls --32 --competition-mode" ++ fn
  withTiming cmd "" $ \ec out err -> do
    logDebug $ "exit code: " <> display (tshow ec)
    logDebug $ "stdout: " <> displayBytesUtf8 out
    -- logDebug $ "stderr: " <> displayBytesUtf8 err
    let lines = C8.lines out
    logDebug $ "lines: " <> display (tshow lines)
    logDebug $ "failed: " <> display (tshow $ "VERIFICATION FAILED" `elem` lines)
    logDebug $ "passed: " <> display (tshow $ "VERIFICATION SUCCESSFUL" `elem` lines)
    case lastMay (C8.lines out) of
         Just "VERIFICATION FAILED"     -> return Sat
         Just "VERIFICATION SUCCESSFUL" -> return Unsat
         l                           -> do
           logWarn $ "unexpected return of 2ls: " <> display (tshow ec) <> "last line: " <> display (tshow l)
           return Unknown




version2Ls :: IO (Maybe String)
version2Ls = Just <$> readCreateProcess (shell "2ls --version") ""

