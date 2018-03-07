module Verifier.Ultimate(uautomizer, utaipan) where

import Verifier.Util

uautomizer :: Verifier
uautomizer = def { verifierName = "uautomizer", execute = run, version = uautomizerVersion}
  where run fn = withSpec reachSafety $ \spec -> do
            let cmd = "Automizer.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec
            putStrLn cmd
            (exitCode, out, _) <- readCreateProcessWithExitCode (shell cmd) ""
            let lastLine = last $ lines out
            case (exitCode, lastLine) of
                (ExitSuccess,"TRUE")  -> return VerificationSuccessful
                (ExitSuccess,"FALSE") -> return VerificationFailed
                _                     -> return VerificationResultUnknown
        uautomizerVersion = Just . head . lines <$> readCreateProcess (shell "Automizer.py --version") ""

utaipan :: Verifier
utaipan = def { verifierName = "utaipan", execute = run, version = uautomizerVersion}
  where run fn = withSpec reachSafety $ \spec -> do
            let cmd = "Taipan.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec
            putStrLn cmd
            (exitCode, out, _) <- readCreateProcessWithExitCode (shell cmd) ""
            let lastLine = last $ lines out
            case (exitCode, lastLine) of
                (ExitSuccess,"TRUE")  -> return VerificationSuccessful
                (ExitSuccess,"FALSE") -> return VerificationFailed
                _                     -> return VerificationResultUnknown
        uautomizerVersion = Just . head . lines <$> readCreateProcess (shell "Taipan.py --version") ""
  
