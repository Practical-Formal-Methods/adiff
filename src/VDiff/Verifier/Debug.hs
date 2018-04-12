-- | defines some 'verifiers' that can be used for debugging
module VDiff.Verifier.Debug
  ( vim
  , alwaysSat
  , alwaysUnsat
  , debuggingVerifiers
  ) where

import           RIO
import           Safe
import           System.Process
import           VDiff.Verifier.Util

debuggingVerifiers :: [Verifier]
debuggingVerifiers = [vim, alwaysSat, alwaysUnsat]

-- | This is not a real verifier. It uses vim to show the file to the user.
-- The user can then say successful by closing vim regularly (:q) or failed by closing vim with non-zero exit code (:cq)
vim :: Verifier
vim = def { verifierName = "vim"
          , execute = runVim
          , version = vimVersion
          }

runVim :: FilePath -> RIO VerifierEnv VerifierResult
runVim fn = do
  let process  = (shell ("vim -R " ++ fn)) {
          std_in        = Inherit
        , std_out       = Inherit
        , delegate_ctlc = False }

  (_,_,_,ph) <- liftIO $ createProcess process
  exitCode <- liftIO $ waitForProcess ph
  case exitCode of
              ExitSuccess   -> return $ VerifierTerminated Unsat def
              ExitFailure _ -> return $ VerifierTerminated Sat def

vimVersion :: IO (Maybe String)
vimVersion = do
  s <- readCreateProcess (shell "vim --version") ""
  return $ headMay $ lines s

alwaysSat, alwaysUnsat :: Verifier
alwaysSat  = always Sat
alwaysUnsat = always Unsat

always :: Verdict -> Verifier
always verdict = def { verifierName = "always-" <> show verdict
                , execute = const $ return (VerifierTerminated verdict def)
                , version = return (Just "1")
                }
