
module Main where

import           RIO

import           VDiff.Arguments   as Args
import           VDiff.Diff
import           VDiff.Persistence
import           VDiff.Types


main :: IO ()
main = do
  params <- execParser opts :: IO MainParameters
  -- set up logging
  logOptions <- logOptionsHandle stderr True
  let logOptions' = setLogMinLevel (logLevel params) logOptions

  -- execute command
  withDiffDB (databaseFn params) $ \database ->
    withLogFunc logOptions' $ \logger -> do
      let env = MainEnv logger database
      runRIO env (runCommands params)


runCommands :: HasMainEnv env => MainParameters -> RIO env ()
runCommands param = case cmd param of
                      (CmdRun dp)        -> cmdDiff dp
                      (CmdParseTest fn)  -> cmdParseTest fn
                      (CmdMarkReads fn)  -> cmdMarkReads fn
                      CmdVersions        -> cmdVersions
                      CmdRunVerifiers dp -> cmdRunVerifiers dp



--------------------------------------------------------------------------------
-- | * Arguments
-- | Arguments are fully typed:
--------------------------------------------------------------------------------

data MainParameters = MainParameters
  { logLevel   :: LogLevel
  , databaseFn :: FilePath
  , cmd        :: Cmd
  }


data Cmd  = CmdRun DiffParameters
          | CmdRunVerifiers DiffParameters
          | CmdParseTest FilePath
          | CmdMarkReads FilePath
          | CmdVersions


--------------------------------------------------------------------------------
-- | * Argument Parser
--------------------------------------------------------------------------------
opts :: ParserInfo MainParameters
opts = info (parseMainParameters <**> helper)
            (fullDesc <> progDesc "vdiff - a simple tool to compare program verifiers")


parseMainParameters :: Parser MainParameters
parseMainParameters = MainParameters <$> level <*> databasePath <*> parseCmd
  where parseCmd = parseCmdVersion <|> parseCmdTest <|> parseCmdMarkReads <|> parseCmdRunDiff <|> parseCmdRunVerifiers





parseCmdVersion :: Parser Cmd
parseCmdVersion = CmdVersions <$ switch ( long "versions" <> help "prints versions of the available verifiers" )

parseCmdTest :: Parser Cmd
parseCmdTest = CmdParseTest <$ switch (long "parse" <> help "parses and prints the given file") <*> cFile

parseCmdMarkReads :: Parser Cmd
parseCmdMarkReads = CmdMarkReads <$ switch (long "mark-reads" <> help "marks the reads in the given file") <*> cFile

parseCmdRunDiff :: Parser Cmd
parseCmdRunDiff = CmdRun <$> Args.diffParameters



parseCmdRunVerifiers :: Parser Cmd
parseCmdRunVerifiers = switch (long "run") *> (CmdRunVerifiers <$> Args.diffParameters)


