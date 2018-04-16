{-# LANGUAGE TemplateHaskell #-}

module VDiff.Verifier.Klee (klee) where

import           RIO
import qualified RIO.ByteString      as BS
import           Safe

import           VDiff.Verifier.Util


klee :: Verifier
klee = def { verifierName = "klee"
           , execute = kleeRun
           , version = kleeVersion
           }

kleeVersion :: IO (Maybe String)
kleeVersion = do
  out <- readCreateProcess (shell "klee --version") ""
  return $ (headMay . lines) out

kleeRun :: FilePath -> RIO VerifierEnv VerifierResult
kleeRun fn = withSystemTempDirectory "kleedir" $ \dir -> do
  -- write klee.h into this directory
  let pathKleeH = dir ++ "/klee.h"
      pathProgram = dir ++ "/program.c"
      pathBC = dir ++ "/program.bc"
  -- save the klee.h header file into this directory
  writeFileBinary pathKleeH kleeH
  -- prepend an include statement
  writeFileUtf8 pathProgram "#include \"klee.h\"\n"
  callCommand ["cat", fn, ">>",  pathProgram]
  -- replace call to __VERIFIER_assert
  callCommand ["sed -i -e", "'s/__VERIFIER_error();/klee_assert(0);/'", pathProgram]
  -- compile with clang
  callCommand ["clang-3.8", "-emit-llvm -c -g",  pathProgram, "-o",  pathBC]

  -- run klee with timing
  withTiming (shell $ "klee " ++pathBC) "" $ \ec _ err -> do
    let hasError = "ASSERTION FAIL" `BS.isInfixOf` err
    case (ec, hasError) of
      (ExitSuccess, True)  -> return Sat
      (ExitSuccess, False) -> return Unsat
      _                    -> do
        logWarn $ "unexpected behaviour of klee (" <> displayShow ec <> ")"
        return Unknown


kleeH :: ByteString
kleeH = $(embedFile "assets/klee.h")
