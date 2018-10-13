module ADiff.Verifier.Ultimate(uautomizer, utaipan) where

import           RIO

import qualified Data.ByteString.Char8 as C8

import           ADiff.Verifier.Util

uautomizer :: Verifier
uautomizer = Verifier "uautomizer" automizerRun uautomizerVersion

utaipan :: Verifier
utaipan = Verifier "utaipan" taipanRun taipanVersion

automizerRun :: FilePath -> RIO VerifierEnv VerifierResult
automizerRun fn = withSpec reachSafety $ \spec ->
  runUltimate $ "Automizer.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec

uautomizerVersion :: IO (Maybe String)
uautomizerVersion = headMay . lines <$> liftIO (readCreateProcess (shell "Automizer.py --version") "")


taipanRun :: FilePath -> RIO VerifierEnv VerifierResult
taipanRun fn = withSpec reachSafety $ \spec ->
            runUltimate $ "Taipan.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec

taipanVersion :: IO (Maybe String)
taipanVersion = headMay . lines <$> readCreateProcess (shell "Taipan.py --version") ""


runUltimate :: String -> RIO VerifierEnv VerifierResult
runUltimate cmd =
  withSystemTempDirectory "ultimate-tmp" $ \dir -> do
    let cmd' = (shell cmd) { cwd = Just dir}
    withTiming cmd' "" $ \ec out err ->
            case (ec, lastMay (C8.lines out))  of
              (ExitSuccess, Just "TRUE")  -> return Unsat
              (ExitSuccess, Just "FALSE") -> return Sat
              _                           -> do
                logWarn $ "unexpected exit code: " <> display (tshow ec)
                  <> ", output was " <> display (tshow out)
                  <> ", error was " <> display (tshow err)
                return Unknown

