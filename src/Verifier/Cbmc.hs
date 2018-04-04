module Verifier.Cbmc (cbmc) where

import qualified Data.ByteString.Char8 as C8
import           RIO
import qualified RIO.List.Partial      as L

import           Verifier.Util

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
  withTiming cmd "" $ \ec out ->
    case (ec, L.last (C8.lines out)) of
         (ExitSuccess,"VERIFICATION FAILED")     -> Sat
         (ExitSuccess,"VERIFICATION SUCCESSFUL") -> Unsat
         _                                       -> Unknown

cbmcVersion :: IO (Maybe String)
cbmcVersion = headMay . lines <$> readCreateProcess (shell "cbmc --version") ""
