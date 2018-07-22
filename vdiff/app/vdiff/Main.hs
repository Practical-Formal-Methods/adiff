module Main where

import           VDiff.Application
import           VDiff.Arguments      as Args
import           VDiff.Diff
import           VDiff.Persistence
import           VDiff.Prelude

import           Control.Applicative  (optional)
import           Control.Monad.Random

infos :: InfoMod a
infos = fullDesc <> progDesc "vdiff - a simple tool to compare program verifiers"

main :: IO ()
main = runVDiffApp parseMainParameters infos $ \case
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

