module Main where

import qualified Data.Text          as T
import qualified Data.Text.IO          as T
import           Prelude            (read)
import           System.Environment
import           VDiff.Execute
import           VDiff.Prelude
import           VDiff.Verifier

import System.IO (openFile)


-- usage: vdiff-run-package <package> <output>
main :: IO ()
main = do
  [packageFp, outputFp] <- getArgs
  (pkg :: ExecutionPackage) <- read . T.unpack <$> readFileUtf8 packageFp
  let (Just v) = lookupVerifier (pkg ^. packageVerifierName)
  withSystemTempFile "program.c" $ \programFp programH -> do
    -- unpack the program
    T.hPutStr programH (pkg ^. inputFile) >> hFlush programH
    let env = VerifierEnv noLog (pkg ^. timelimit) (pkg ^. verifierExtraFlags)
    result <- runRIO env $ executeVerifier v programFp
    outH <- openFile outputFp WriteMode
    T.hPutStr outH (T.pack $ show result)
    hFlush outH
    hClose outH

  where
    noLog = mkLogFunc $ \_ _ _ db -> T.putStrLn (utf8BuilderToText db)
