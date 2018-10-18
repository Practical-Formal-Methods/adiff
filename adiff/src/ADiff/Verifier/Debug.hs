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

-- | defines some 'verifiers' that can be used for debugging
module ADiff.Verifier.Debug
  ( vim
  , alwaysSat
  , alwaysUnsat
  , debuggingVerifiers
  ) where

import           RIO
import           Safe
import           System.Process
import           ADiff.Data
import           ADiff.Verifier.Util

debuggingVerifiers :: [Verifier]
debuggingVerifiers = [vim, vim2, alwaysSat, alwaysUnsat]

-- | This is not a real verifier. It uses vim to show the file to the user.
-- The user can then say successful by closing vim regularly (:q) or failed by closing vim with non-zero exit code (:cq)
vim :: Verifier
vim = Verifier "vim" runVim vimVersion

vim2 :: Verifier
vim2 = Verifier "vim2" runVim vimVersion


runVim :: FilePath -> RIO VerifierEnv VerifierResult
runVim fn = do
  let process  = (shell ("vim -R " ++ fn)) {
          std_in        = Inherit
        , std_out       = Inherit
        , delegate_ctlc = False }

  (_,_,_,ph) <- liftIO $ createProcess process
  exitCode <- liftIO $ waitForProcess ph
  case exitCode of
              ExitSuccess   -> return $ VerifierResult (Just 1.0) (Just 100) Unsat
              ExitFailure _ -> return $ VerifierResult (Just 1.0) (Just 100) Sat

vimVersion :: IO (Maybe String)
vimVersion = headMay . lines <$> readCreateProcess (shell "vim --version") ""

alwaysSat, alwaysUnsat :: Verifier
alwaysSat  = always Sat
alwaysUnsat = always Unsat

always :: Verdict -> Verifier
always v = Verifier ("always-" <> tshow v) exe version
  where
    exe = const $ return (VerifierResult (Just 1.0) (Just 100) v)
    version = return (Just "1")
