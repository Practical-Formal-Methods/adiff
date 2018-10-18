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

module ADiff.Verifier.Cbmc (cbmc) where

import qualified Data.ByteString.Char8 as C8
import           RIO
import qualified RIO.List.Partial      as L

import           ADiff.Verifier.Util

-- | This is the cbmc verifier. The last line of its output on stdout tells us
-- the result of the verification.
cbmc :: Verifier
cbmc = Verifier "cbmc" runCbmc cbmcVersion

runCbmc :: FilePath -> RIO VerifierEnv VerifierResult
runCbmc fn = do
  let cmd = shell $ "cbmc --32 --error-label ERROR " ++ fn
  withTiming cmd "" $ \ec out _ ->
    case L.last (C8.lines out) of
         "VERIFICATION FAILED"     -> return Sat
         "VERIFICATION SUCCESSFUL" -> return Unsat
         l                         -> do
           logWarn $ "unexpected return of cbmc: " <> display (tshow ec) <> "last line: " <> display (tshow l)
           return Unknown

cbmcVersion :: IO (Maybe String)
cbmcVersion = headMay . lines <$> readCreateProcess (shell "cbmc --version") ""
