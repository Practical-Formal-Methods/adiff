module Verifier.Ultimate(uautomizer, utaipan) where

import           RIO

import           Verifier.Util

uautomizer :: Verifier
uautomizer = def { verifierName = "uautomizer"
                 , execute = automizerRun
                 , version = uautomizerVersion
                 }

utaipan :: Verifier
utaipan = def { verifierName = "utaipan"
              , execute = taipanRun
              , version = taipanVersion
              }

automizerRun :: FilePath -> RIO VerifierEnv (VerifierResult, Timing)
automizerRun fn = withSpec reachSafety $ \spec ->
  runUltimate $ "Automizer.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec

uautomizerVersion :: IO (Maybe String)
uautomizerVersion = headMay . lines <$> liftIO (readCreateProcess (shell "Automizer.py --version") "")


taipanRun :: FilePath -> RIO VerifierEnv (VerifierResult, Timing)
taipanRun fn = withSpec reachSafety $ \spec ->
            runUltimate $ "Taipan.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec

taipanVersion :: IO (Maybe String)
taipanVersion = headMay . lines <$> readCreateProcess (shell "Taipan.py --version") ""


runUltimate :: String -> RIO VerifierEnv (VerifierResult, Timing)
runUltimate cmd = do
            (exitCode, out, timing) <- execTimed (shell cmd) ""
            let lastLine = lastMay $ lines out
            case (exitCode, lastLine) of
                (ExitSuccess, Just "TRUE")  -> return (VerificationSuccessful, timing)
                (ExitSuccess, Just "FALSE") -> return (VerificationFailed, timing)
                (status, line)  -> do
                  logDebug $ "ultimate verifier exited with " <> displayShow (status, line)
                  return (VerificationResultUnknown, timing)

