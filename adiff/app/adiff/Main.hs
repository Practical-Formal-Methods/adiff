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

import           ADiff.Application
import           ADiff.Arguments      as Args
import           ADiff.Diff
import           ADiff.Persistence
import           ADiff.Prelude

import           Control.Applicative  (optional)
import           Control.Monad.Random

infos :: InfoMod a
infos = fullDesc <> progDesc "adiff - a simple tool to compare program verifiers"

main :: IO ()
main = runADiffApp parseMainParameters infos $ \case
  (CmdRun seed dp)       -> cmdDiff seed dp
  (CmdParseTest fn)      -> cmdParseTest fn
  (CmdMarkReads mode fn) -> cmdMarkReads mode fn
  CmdVersions            -> cmdVersions
  CmdRunVerifiers dp     -> cmdRunVerifiers dp


data Cmd  = CmdRun (Maybe Int) DiffParameters -- first parameter is the seed
          | CmdRunVerifiers DiffParameters
          | CmdParseTest FilePath
          | CmdMarkReads SearchMode FilePath
          | CmdVersions


parseMainParameters :: Parser Cmd
parseMainParameters = hsubparser $ mconcat [versionCommand, parseCommand, markCommand, runCommand, diffCommand]
  where
    versionCommand = command "versions" (info (pure CmdVersions) (progDesc "display the versions of the integrated verifiers"))
    parseCommand   = command "parse" (info (CmdParseTest <$> cFile) (progDesc "parses and prints the given file"))
    markCommand    = command "mark-reads" (info (CmdMarkReads <$> Args.searchMode <*> cFile) (progDesc "marks the reads in the given file"))
    runCommand     = command "run" (info (CmdRunVerifiers <$> Args.diffParameters) (progDesc "execute verifiers"))
    diffCommand    = command "diff" (info (CmdRun <$> Args.seed <*> Args.diffParameters) (progDesc "diff verifiers"))

