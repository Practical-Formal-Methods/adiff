module Verifier.Ultimate(uautomizer, utaipan) where

import           RIO
import qualified RIO.List      as L
import           System.IO     (putStrLn)

import           Verifier.Util

uautomizer :: Verifier
uautomizer = def { verifierName = "uautomizer", execute = run, version = uautomizerVersion}
  where run fn = withSpec reachSafety $ \spec ->
            runUltimate $ "Automizer.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec

        uautomizerVersion = Just . L.head . lines <$> readCreateProcess (shell "Automizer.py --version") ""

utaipan :: Verifier
utaipan = def { verifierName = "utaipan", execute = run, version = uautomizerVersion}
  where run fn = withSpec reachSafety $ \spec ->
            runUltimate $ "Taipan.py --architecture 32bit --file " ++ fn ++ " --spec " ++ spec

        uautomizerVersion = Just . L.head . lines <$> readCreateProcess (shell "Taipan.py --version") ""


runUltimate :: String -> IO (VerifierResult, Timing)
runUltimate cmd = do
            -- putStrLn cmd
            (exitCode, out, timing) <- execTimed (shell cmd) ""
            let lastLine = L.last $ lines out
            case (exitCode, lastLine) of
                (ExitSuccess,"TRUE")  -> return (VerificationSuccessful, timing)
                (ExitSuccess,"FALSE") -> return (VerificationFailed, timing)
                (status, line)  -> do
                  putStrLn $ "ultimate verifier exited with " ++ show (status, line)
                  return (VerificationResultUnknown, timing)

