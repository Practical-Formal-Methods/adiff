{-# LANGUAGE TemplateHaskell #-}

module Verifier.Klee (klee) where

import           RIO
import qualified RIO.ByteString        as BS
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

kleeRun :: FilePath -> RIO VerifierEnv VerifierResult
kleeRun fn = withKleeH $ \kleeH ->
    withSystemTempFile "file.bc" $ \bc _ -> do
      liftIO $ callCommand $ "clang-3.8 -emit-llvm -I " ++ kleeH ++ " -c -g " ++ fn ++ " -o " ++ bc
      let cmd = shell $ "klee " ++ bc
      withTiming cmd "" $ \ec out ->
        case (ec, BS.null out) of
          (ExitSuccess, True)  -> return Unsat
          (ExitSuccess, False) -> return Sat
          _                    -> error "unexpected outcome in klee"


withKleeH :: (MonadUnliftIO m) => (FilePath -> m a) -> m a
withKleeH actn = withSystemTempFile "klee.h"$ \f h -> do
  liftIO $ C8.hPutStrLn h $(embedFile "assets/klee.h")
  liftIO $ hFlush h
  actn f
