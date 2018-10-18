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

{-# LANGUAGE OverloadedStrings #-}

module ADiff.Verifier.CrabLlvm where


import qualified Data.ByteString.Char8 as C8
import           Data.Either
import qualified Data.Text             as T
import qualified Prelude               as P
import qualified RIO.List              as L
import           Text.Regex.Posix
import           ADiff.Prelude

import           ADiff.Verifier.Util

crabLlvm :: Verifier
crabLlvm = Verifier "crab-llvm" executeCrabLlvm crabVersion

-- | Apparently, the script "sea" has to be run from the directory it is installed in.
-- At the moment, the path /verifiers/crab-llvm/bin is hard coded. TODO: Use which or similar to change cwd based on that.
executeCrabLlvm :: FilePath -> RIO VerifierEnv VerifierResult
executeCrabLlvm fn = do
  flgs <- map T.unpack <$> view extraFlags
  let cmd = (shell $ "./crabllvm.py -m 32 --crab-inter --crab-check=assert " ++ L.intercalate " " flgs ++ " "  ++ fn) {cwd = Just "/verifiers/crab-llvm/bin"}
  withTiming cmd "" $ \_ out _ -> do
    let (x,wrn,err) = extractNumbers out
    if wrn == 0 && err == 0
      then return Unsat
      else return Sat

crabVersion :: IO (Maybe String)
crabVersion = return Nothing

extractNumbers :: ByteString -> (Int, Int, Int)
extractNumbers out =
    let [ln_safe, ln_error, ln_total] = take 3 . drop 1 . dropWhile (\l -> not (l =~ ("^(\\*)+ ANALYSIS RESULTS .*" :: ByteString))) . C8.lines $ out
        [safe, warning, error]  = P.read . C8.unpack . P.head . C8.words <$> [ln_safe, ln_error, ln_total]
    in (safe,warning,error)

