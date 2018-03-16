{-# LANGUAGE TemplateHaskell #-}

module Verifier.Klee (klee) where

import           Verifier.Util

import qualified Data.ByteString.Char8   as C8

klee :: Verifier
klee = def { verifierName = "klee", execute = kleeExecute, version = kleeVersion}
  where
    kleeVersion = do
      (_,out,_) <- readCreateProcessWithExitCode (shell "klee --version") ""
      return $ (Just . head . lines) out

kleeExecute :: FilePath -> IO (VerifierResult, Timing)
kleeExecute fn = withKleeH $ \kleeH ->
    withSystemTempFile "file.bc" $ \bc _ -> do
    callCommand $ "clang -emit-llvm -I " ++ kleeH ++ " -c -g " ++ fn ++ " -o " ++ bc
    (exitCode, out, timing) <- execTimed (shell $ "klee " ++ bc) ""
    case (exitCode, null out) of
      (ExitSuccess, True)  -> return (VerificationSuccessful, timing)
      (ExitSuccess, False) -> return (VerificationFailed, timing)
      (ExitFailure _, _ )  -> return (VerificationResultUnknown, timing)


withKleeH :: (FilePath -> IO a) -> IO a
withKleeH actn = withSystemTempFile "klee.h"$ \f h -> do
  C8.hPutStrLn h $(embedFile "assets/klee.h")
  hFlush h
  actn f
