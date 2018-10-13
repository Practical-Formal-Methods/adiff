{-# LANGUAGE TemplateHaskell #-}

module ADiff.Verifier.Klee (klee) where

import           Data.FileEmbed
import           RIO
import qualified RIO.ByteString      as BS
import           Safe

import           ADiff.Verifier.Util



klee :: Verifier
klee = Verifier "klee" kleeRun kleeVersion

kleeVersion :: IO (Maybe String)
kleeVersion = headMay . lines <$> readCreateProcess (shell "klee --version") ""

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
  callCommand ["clang-3.8", "-emit-llvm -O0 -c -g",  pathProgram, "-o",  pathBC]

  -- run klee with timing
  let cmd = shell $ "klee -silent-klee-assume --search=dfs -max-forks=64 " ++ pathBC
  withTiming cmd "" $ \ec _ err -> do
    let hasError = "ASSERTION FAIL" `BS.isInfixOf` err
    case (ec, hasError) of
      (ExitSuccess, True)  -> return Sat
      (ExitSuccess, False) -> return Unsat
      _                    -> do
        logWarn $ "unexpected behaviour of klee (" <> displayShow ec <> ")"
        return Unknown


kleeH :: ByteString
kleeH = $(embedOneFileOf ["assets/klee.h", "adiff/assets/klee.h"])
