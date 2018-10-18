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

module ADiff.Verifier.CpaChecker (cpaChecker) where

import           RIO


import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.List             (isPrefixOf)
import           Data.Maybe            (listToMaybe)

import           ADiff.Verifier.Util

cpaChecker :: Verifier
cpaChecker = Verifier "cpachecker" cpaExecute cpaVersion

cpaExecute :: FilePath -> RIO VerifierEnv VerifierResult
cpaExecute fn = withSpec reachSafety $ \spec -> do
  let cmd = shell $ "cpa.sh -32 -default -nolog -noout -spec " ++ spec ++ " " ++ fn
  withTiming cmd "" $ \_ out _ -> do
      let out' = filter ("Verification result" `BS.isPrefixOf`) $ C8.lines out
      let verdict = case out' of
            (s:_) | "Verification result: TRUE" `BS.isPrefixOf` s -> Unsat
                  | "Verification result: FALSE" `BS.isPrefixOf` s -> Sat
            _ -> Unknown
      return verdict


cpaVersion :: IO (Maybe String)
cpaVersion = do
    out <- readCreateProcess (shell "cpa.sh -h") ""
    let v = listToMaybe $ filter (\l -> "CPAchecker" `isPrefixOf` l) $ lines out
    return v

