
module Verifier.Vim (vim) where

import           RIO
import           Safe
import           Verifier.Util

-- | This is not a real verifier. It uses vim to show the file to the user.
-- The user can then say successful by closing vim regularly (:q) or failed by closing vim with non-zero exit code (:cq)
vim :: Verifier
vim = def { verifierName = "vim", execute = runVim, version = vimVersion }

runVim :: FilePath -> IO (VerifierResult, Timing)
runVim fn = do
  let process  = (shell ("vim -R " ++ fn)) {
          std_in        = Inherit
        , std_out       = Inherit
        , delegate_ctlc = False }

  (_,_,_,ph) <- createProcess process
  exitCode <- waitForProcess ph
  case exitCode of
              ExitSuccess   -> return (VerificationSuccessful, def)
              ExitFailure _ -> return (VerificationFailed, def)

vimVersion :: IO (Maybe String)
vimVersion = do
  s <- readCreateProcess (shell "vim --version") ""
  return $ headMay $ lines s
