module Verifier.Ultimate(uautomizer, utaipan) where

import           RIO

import           Verifier.Util

import qualified Data.ByteString.Char8 as C8

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
            (termination, out, _) <- execTimed ((shell cmd) {cwd = Just dir}) ""
            case (termination, lastMay (C8.lines out))  of
              (Nothing, _) -> return VerifierTimedOut
              (Just (ExitSuccess, timing), Just "TRUE")  -> return $ VerifierTerminated Unsat timing
              (Just (ExitSuccess, timing), Just "FALSE") -> return $ VerifierTerminated Sat timing
              (Just (_, timing), _)                      -> return $ VerifierTerminated Unknown timing

