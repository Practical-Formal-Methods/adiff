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

module ADiff.Verifier.Ultimate(uautomizer, utaipan) where

import           RIO

import qualified Data.ByteString.Char8 as C8

import           ADiff.Verifier.Util

uautomizer :: Verifier
uautomizer = Verifier "uautomizer" automizerRun uautomizerVersion

utaipan :: Verifier
utaipan = Verifier "utaipan" taipanRun taipanVersion

automizerRun :: FilePath -> RIO VerifierEnv VerifierResult
automizerRun fn = withSpec reachSafety $ \spec ->
  runUltimate $ "Automizer.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec

uautomizerVersion :: IO (Maybe String)
uautomizerVersion = headMay . lines <$> liftIO (readCreateProcess (shell "Automizer.py --version") "")


taipanRun :: FilePath -> RIO VerifierEnv VerifierResult
taipanRun fn = withSpec reachSafety $ \spec ->
            runUltimate $ "Taipan.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec

taipanVersion :: IO (Maybe String)
taipanVersion = headMay . lines <$> readCreateProcess (shell "Taipan.py --version") ""


runUltimate :: String -> RIO VerifierEnv VerifierResult
runUltimate cmd =
  withSystemTempDirectory "ultimate-tmp" $ \dir -> do
    let cmd' = (shell cmd) { cwd = Just dir}
    withTiming cmd' "" $ \ec out err ->
            case (ec, lastMay (C8.lines out))  of
              (ExitSuccess, Just "TRUE")  -> return Unsat
              (ExitSuccess, Just "FALSE") -> return Sat
              _                           -> do
                logWarn $ "unexpected exit code: " <> display (tshow ec)
                  <> ", output was " <> display (tshow out)
                  <> ", error was " <> display (tshow err)
                return Unknown

