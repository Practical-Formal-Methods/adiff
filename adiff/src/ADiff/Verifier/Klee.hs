-- MIT License
--
-- Copyright (c) 2018 Christian Klinger
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

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
