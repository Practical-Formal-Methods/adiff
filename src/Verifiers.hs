module Verifiers
  ( VerifierResult
  , Verifier(..)
  , allVerifiers
  ) where

import           Data.Monoid    ((<>))
import           System.Exit
import           System.Process


-- | TODO: Add execution time and more
data VerifierResult = VerificationSuccessful | VerificationFailed
  deriving (Show, Eq)

data Verifier = Verifier
  { verifierName :: String
  , execute      :: FilePath -> IO VerifierResult
  }


allVerifiers :: [Verifier]
allVerifiers = [cbmc, cbmcPrime]

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

