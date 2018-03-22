{-# LANGUAGE TemplateHaskell #-}

module Verifier.Klee (klee) where

import           RIO
import           Safe
import           Verifier.Util

import qualified Data.ByteString.Char8 as C8

klee :: Verifier
klee = def { verifierName = "klee"
           , execute = kleeRun
           , version = kleeVersion
           }

kleeVersion :: IO (Maybe String)
kleeVersion = do
  (_,out,_) <- readCreateProcessWithExitCode (shell "klee --version") ""
  return $ (headMay . lines) out

kleeRun :: FilePath -> RIO VerifierEnv (VerifierResult, Timing)
kleeRun fn = withKleeH $ \kleeH ->
    withSystemTempFile "file.bc" $ \bc _ -> do
      liftIO $ callCommand $ "clang -emit-llvm -I " ++ kleeH ++ " -c -g " ++ fn ++ " -o " ++ bc
      (exitCode, out, _, timing) <- execTimed (shell $ "klee " ++ bc) ""
      case (exitCode, null out) of
        (ExitSuccess, True)  -> return (VerificationSuccessful, timing)
        (ExitSuccess, False) -> return (VerificationFailed, timing)
        (ExitFailure _, _ )  -> return (VerificationResultUnknown, timing)


withKleeH :: (MonadUnliftIO m) => (FilePath -> m a) -> m a
withKleeH actn = withSystemTempFile "klee.h"$ \f h -> do
  liftIO $ C8.hPutStrLn h $(embedFile "assets/klee.h")
  liftIO $ hFlush h
  actn f
