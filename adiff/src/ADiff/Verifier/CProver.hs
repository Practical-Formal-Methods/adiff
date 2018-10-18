-- MIT License
--
-- Copyright (c) 2018 Christian Klinger
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE MultiWayIf #-}
module ADiff.Verifier.CProver
 ( cprover2Ls
 ) where

import qualified Data.ByteString.Char8 as C8
import           RIO
import           Safe
import           ADiff.Verifier.Util

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

