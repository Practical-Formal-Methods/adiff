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

module Main where

import qualified Data.Text          as T
import qualified Data.Text.IO          as T
import           Prelude            (read)
import           System.Environment
import           ADiff.Execute
import           ADiff.Prelude
import           ADiff.Verifier

import System.IO (openFile)


-- usage: adiff-run-package <package> <output>
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
