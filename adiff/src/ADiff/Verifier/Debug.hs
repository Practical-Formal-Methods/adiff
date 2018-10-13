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
