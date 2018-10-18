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

module ADiff.Verifier.Sea(seahorn, seacrab) where

import           ADiff.Prelude

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text             as T
import qualified RIO.List              as L

import           ADiff.Verifier.Util

seahorn :: Verifier
seahorn = Verifier "seahorn" (runSea False) seaVersion

seacrab :: Verifier
seacrab = Verifier "seacrab" (runSea True) seaVersion

-- | Apparently, the script "sea" has to be run from the directory it is installed in.
-- At the moment, the path /verifiers/seahorn/bin is hard coded. TODO: Use which or similar to change cwd based on that.
runSea :: Bool -> FilePath -> RIO VerifierEnv VerifierResult
runSea enableCrab fn = do
  flgs <- map T.unpack <$> view extraFlags
  let crabFlag = if enableCrab then "--crab" else ""
      cmd = (shell $ "./sea " ++ crabFlag ++ " " ++ L.intercalate " " flgs ++ " pf "  ++ fn) {cwd = Just "/verifiers/seahorn/bin"}
  withTiming cmd "" $ \_ out _ ->
    case lastMay (C8.lines out) of
      Just "sat"   -> return Sat
      Just "unsat" -> return Unsat
      _            -> return Unknown

seaVersion :: IO (Maybe String)
seaVersion = return Nothing
