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
main = runVDiffApp parseMainParameters infos $ \params -> do
  -- initialize random
  s <- case seed params of
    Just s  -> return s
    Nothing -> getRandomR (1,10000)
  logInfo $ "seed for random generator: " <> display s
  liftIO $ setStdGen $ mkStdGen s
  -- execute command
  runCommands params


runCommands :: HasMainEnv env => MainParameters -> RIO env ()
runCommands param = case cmd param of
                      (CmdRun dp)            -> cmdDiff dp
                      (CmdParseTest fn)      -> cmdParseTest fn
                      (CmdMarkReads mode fn) -> cmdMarkReads mode fn
                      CmdVersions            -> cmdVersions
                      CmdRunVerifiers dp     -> cmdRunVerifiers dp



--------------------------------------------------------------------------------
-- | * Arguments
-- | Arguments are fully typed:
--------------------------------------------------------------------------------

data MainParameters = MainParameters
  { seed :: Maybe Int
  , cmd  :: Cmd
  }


data Cmd  = CmdRun DiffParameters
          | CmdRunVerifiers DiffParameters
          | CmdParseTest FilePath
          | CmdMarkReads SearchMode FilePath
          | CmdVersions


--------------------------------------------------------------------------------
-- | * Argument Parser
--------------------------------------------------------------------------------

parseMainParameters :: Parser MainParameters
parseMainParameters = MainParameters <$> parseSeed <*> parseCmd
  where parseCmd = parseCmdVersion <|> parseCmdTest <|> parseCmdMarkReads <|> parseCmdRunDiff <|> parseCmdRunVerifiers


parseSeed :: Parser (Maybe Int)
parseSeed = optional $ option auto options
  where options = mconcat [ long "seed"
                          , help "seed to initialize random generator"
                          , metavar "SEED"
                          ]


parseCmdVersion :: Parser Cmd
parseCmdVersion = CmdVersions <$ switch ( long "versions" <> help "prints versions of the available verifiers" )

parseCmdTest :: Parser Cmd
parseCmdTest = CmdParseTest <$ switch (long "parse" <> help "parses and prints the given file") <*> cFile

parseCmdMarkReads :: Parser Cmd
parseCmdMarkReads = CmdMarkReads <$ switch (long "mark-reads" <> help "marks the reads in the given file") <*> Args.searchMode <*> cFile

parseCmdRunDiff :: Parser Cmd
parseCmdRunDiff = CmdRun <$> Args.diffParameters



parseCmdRunVerifiers :: Parser Cmd
parseCmdRunVerifiers = switch (long "run") *> (CmdRunVerifiers <$> Args.diffParameters)


