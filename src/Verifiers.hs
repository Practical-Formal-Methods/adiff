module Verifiers
  ( VerifierResult
  , Verifier(..)
  , allVerifiers
  ) where

import           Data.Monoid    ((<>))
import           System.Exit
import           System.Process

import           Types

allVerifiers :: [Verifier]
allVerifiers = [cbmc, cbmcPrime, vim]

-- | This is the cbmc verifier. The last line of its output on stdout tells us
-- the result of the verification.
cbmc :: Verifier
cbmc = Verifier "cbmc" run
  where run fn = do
          let cmd = "cbmc --32 " <> fn
          (exitCode, out, _) <- readCreateProcessWithExitCode (shell cmd) ""
          let lastLine = last $ lines out
          case (exitCode, lastLine) of
              (ExitSuccess,"VERIFICATION FAILED")     -> return VerificationFailed
              (ExitSuccess,"VERIFICATION SUCCESSFUL") -> return VerificationSuccessful
              _                                       -> return VerificationFailed


-- | At the moment I don't have another verifier ready to use
cbmcPrime :: Verifier
cbmcPrime = Verifier "cbmc-prime" (execute cbmc)


-- | This is not a real verifier. It uses vim to show the file to the user.
-- The user can then say successful by closing vim regularly (:q) or failed by closing vim with non-zero exit code (:cq)
vim :: Verifier
vim = Verifier "vim" run
  where run fn = do
          let process  = shell ("vim -R" <> fn) {
                  std_in        = Inherit
                , std_out       = Inherit
                , delegate_ctlc = False }

          (_,_,_,ph) <- createProcess process
          exitCode <- waitForProcess ph
          case exitCode of
            ExitSuccess   -> return VerificationSuccessful
            ExitFailure _ -> return VerificationFailed
