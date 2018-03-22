{-# LANGUAGE LambdaCase #-}

module Main where

import           RIO
import qualified RIO.List            as L

import           Options.Applicative

import           Diff
import           Persistence
import           Types
import           Verifier


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
                      (CmdRun dp)       -> cmdDiff dp
                      (CmdParseTest fn) -> cmdParseTest fn
                      CmdVersions       -> cmdVersions



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
          | CmdParseTest FilePath
          | CmdVersions


--------------------------------------------------------------------------------
-- | * Argument Parser
--------------------------------------------------------------------------------
opts :: ParserInfo MainParameters
opts = info (parseMainParameters <**> helper)
            (fullDesc <> progDesc "vdiff - a simple tool to compare program verifiers")


parseMainParameters :: Parser MainParameters
parseMainParameters = MainParameters <$> parseLogLevel <*> parseDatabasePath <*> parseCmd
  where parseCmd = parseCmdVersion <|> parseCmdTest <|> parseCmdRun

parseLogLevel :: Parser LogLevel
parseLogLevel = option levelP (long "log-level" <> help helpText <> metavar "LOGLEVEL" <> value LevelWarn)
  where
    helpText = "Allowed values: debug, info, warning, error"
    levelP = (str :: ReadM Text ) >>= \case
      "debug"   -> return LevelDebug
      "info"    -> return LevelInfo
      "warning" -> return LevelWarn
      "error"   -> return LevelError
      s -> readerError $ "unknown log level " ++ show s


parseDatabasePath :: Parser String
parseDatabasePath = option str (long "database" <> short 'd' <> help "path to sqlite3 database" <> value ":memory:")


parseCmdVersion :: Parser Cmd
parseCmdVersion = CmdVersions <$ switch ( long "versions" <> help "prints versions of the available verifiers" )

parseCmdTest :: Parser Cmd
parseCmdTest = CmdParseTest <$ switch (long "parse" <> help "parses and prints the given file") <*> argument str (metavar "FILE")

parseCmdRun :: Parser Cmd
parseCmdRun = CmdRun <$> (DiffParameters
      <$> switch ( long "verbose" <> help "verbose output")
      <*> option stratParser (mconcat [ long "strategy"
                                      , help "guidance algorithm"
                                      , value NaiveRandom
                                      , showDefaultWith strategyName
                                      , metavar "STRATEGY"])
      <*> option auto ( long "iterations" <> short 'n' <> help "number of iterations" <> value 1)
      <*> option verifierParser (mconcat [ long "verifiers"
                                        , help ("the compared verifiers (available: " ++ show (map verifierName allVerifiers) ++ ")"  )
                                        , value []
                                        , metavar "VERIFIERS"
                                        ])
      <*> argument str (metavar "FILE"))

stratParser :: ReadM Strategy
stratParser = (str :: ReadM Text) >>= \case
    "naive"        -> return NaiveRandom
    "smart"          -> return SmartGuided
    _ -> readerError "Accepted strategies are 'naive' and 'smart'."

verifierParser :: ReadM [Verifier]
verifierParser = str >>= \s -> if s == ""
                               then pure []
                               else let reqVer = words s
                                        unavailable = reqVer L.\\ map verifierName allVerifiers
                                    in
                                      if null unavailable
                                      then pure $ filter (\v -> verifierName v `elem` reqVer) allVerifiers
                                      else readerError $ "unknown verifier(s): " ++ unwords unavailable

