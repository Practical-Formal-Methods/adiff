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
  (CmdRun tc seed dp)       -> cmdDiff tc seed dp
  (CmdParseTest tc fn)      -> cmdParseTest tc fn
  (CmdMarkReads mode tc fn) -> cmdMarkReads mode tc fn
  CmdVersions            -> cmdVersions
  CmdRunVerifiers dp     -> cmdRunVerifiers dp


data Cmd  = CmdRun TypecheckerFlag (Maybe Int) DiffParameters -- first parameter is the seed
          | CmdRunVerifiers DiffParameters
          | CmdParseTest TypecheckerFlag FilePath
          | CmdMarkReads SearchMode TypecheckerFlag FilePath
          | CmdVersions


parseMainParameters :: Parser Cmd
parseMainParameters = hsubparser $ mconcat [versionCommand, parseCommand, markCommand, runCommand, diffCommand]
  where
    versionCommand = command "versions" (info (pure CmdVersions) (progDesc "display the versions of the integrated verifiers"))
    parseCommand   = command "parse" (info (CmdParseTest <$> Args.typechecker <*> cFile) (progDesc "parses and prints the given file"))
    markCommand    = command "mark-reads" (info (CmdMarkReads <$> Args.searchMode <*> Args.typechecker <*> cFile) (progDesc "marks the reads in the given file"))
    runCommand     = command "run" (info (CmdRunVerifiers <$> Args.diffParameters) (progDesc "execute verifiers"))
    diffCommand    = command "diff" (info (CmdRun <$> Args.typechecker <*> Args.seed <*> Args.diffParameters) (progDesc "diff verifiers"))

