{-# LANGUAGE RankNTypes #-}

{- Building blocks for applications -}
module VDiff.Prelude.Internal.Application where

import RIO
import Options.Applicative
import qualified Data.List as L
import qualified Database.SQLite.Simple         as SQL
import           Database.Beam
import           Database.Beam.Sqlite           as Sqlite
import VDiff.Data

import VDiff.Prelude.Types

runVDiffApp :: (Parser p) -> (forall b. InfoMod b) -> (p -> RIO MainEnv a) -> IO a
runVDiffApp parser infoMod app = do
  -- parse arguments
  let combinedParser = (,) <$> parseMainArguments <*> parser
  ((dbPath, logLevel), customParams) <- execParser (info (combinedParser <**> helper) infoMod)

  -- set up logging
  logOptions <- logOptionsHandle stderr True
  let logOptions' = setLogMinLevel logLevel logOptions

  -- wrap app in withLogFunc and withDiffDB
  withLogFunc logOptions' $ \logger ->
    withDiffDB dbPath $ \database -> do
      let env = MainEnv logger database
      runRIO env (app customParams)


-- | parses loglevel, database
parseMainArguments :: Parser (FilePath, LogLevel)
parseMainArguments = (,) <$> databasePath <*> level


level :: Parser LogLevel
level = option levelP options
  where
    options = mconcat [ long "log-level"
                      , help helpText
                      , metavar "LOGLEVEL"
                      , value LevelWarn
                      , completeWith values
                      ]
    values = ["debug", "info", "warning", "error"]
    helpText = "Allowed values: " ++ concat (L.intersperse ", " values)
    levelP = (str :: ReadM Text ) >>= \case
      "debug"   -> return LevelDebug
      "info"    -> return LevelInfo
      "warning" -> return LevelWarn
      "error"   -> return LevelError
      s -> readerError $ "unknown log-level " ++ show s

databasePath :: Parser String
databasePath = option str options
  where options = mconcat [ long "database"
                          , short 'd'
                          , help "path to sqlite3 database (if not given, an in-memory database is used)"
                          , value ":memory:"
                          , action "file"
                          ]

-- | initializes the database with the necessary parameters. If not file path is given, the default name "vdiff.db" is used.
withDiffDB :: FilePath -> (SQL.Connection -> IO a) -> IO a
withDiffDB fn act = do
  conn <- liftIO $ SQL.open fn
  -- migrate / create schema
  runBeamSqlite conn migrateVdiff
  x <- act conn
  SQL.close conn
  return x
