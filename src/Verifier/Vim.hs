module Verifier.Vim (vim) where

import Verifier.Util

-- | This is not a real verifier. It uses vim to show the file to the user.
-- The user can then say successful by closing vim regularly (:q) or failed by closing vim with non-zero exit code (:cq)
vim :: Verifier
vim = def { verifierName = "vim", execute = run, version = vimVersion }
  where run fn = do
          let process  = (shell ("vim -R " ++ fn)) {
                  std_in        = Inherit
                , std_out       = Inherit
                , delegate_ctlc = False }

          (_,_,_,ph) <- createProcess process
          exitCode <- waitForProcess ph
          case exitCode of
            ExitSuccess   -> return VerificationSuccessful
            ExitFailure _ -> return VerificationFailed
        vimVersion = do
          s <- readCreateProcess (shell "vim --version") ""
          return $ Just <$> head $ lines s
