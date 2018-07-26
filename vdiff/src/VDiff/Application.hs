{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{- Building blocks for applications -}
module VDiff.Application where

import qualified Data.List              as L
import           Data.Pool
import qualified Data.Text              as T
import           Database.Beam
import           Database.Beam.Sqlite   as Sqlite
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite3       as Sqlite3
import           Options.Applicative
import           RIO
import           System.IO              (putStrLn)
import           VDiff.Data

import           VDiff.Persistence      (retryOnBusy, runBeam)
import           VDiff.Prelude.Types

runVDiffApp :: Parser p -> (forall b. InfoMod b) -> (p -> RIO MainEnv a) -> IO a
runVDiffApp parser infoMod app = do
  -- parse arguments
  let combinedParser = (,) <$> parseMainArguments <*> parser
  ((dbPath, inMemoryFlag, bkpPath, logLevel), customParams) <- execParser (info (combinedParser <**> helper) infoMod)

  -- set up logging
  logOptions <- logOptionsHandle stderr True
  let logOptions' = setLogMinLevel logLevel logOptions

  let dbPath' = if inMemoryFlag then ":memory:" else dbPath
  -- wrap app in withLogFunc and withDiffDB
  withLogFunc logOptions' $ \logger ->
    withDiffDB dbPath' $ \pool -> do
      let env = MainEnv logger pool
      runRIO env $ do
        -- if necessary load into memory
        when inMemoryFlag (restoreDb (T.pack dbPath))
        x <- app customParams
        -- run backup if requested
        case bkpPath of
          Nothing  -> return ()
          Just bkp -> backupDb (T.pack bkp)
        return x


-- | parses loglevel, database
parseMainArguments :: Parser (FilePath, Bool, Maybe FilePath, LogLevel)
parseMainArguments = (,,,) <$> databasePath <*> inMemory <*> databaseBkpPath <*> level


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
    helpText = "Allowed values: " ++ L.intercalate ", " values
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

databaseBkpPath :: Parser (Maybe String)
databaseBkpPath = option (Just <$> str) options
  where options = mconcat [ long "backup"
                          , help "path to sqlite3 backup database (optional, if given, performs backup)"
                          , action "file"
                          , value Nothing
                          ]

inMemory :: Parser Bool
inMemory = switch (long "in-memory" <> help "WARNING: potential data loss")

-- | Initializes the database with the necessary parameters, migrates the schema
-- and creates indices. For sqlite the number of concurrent connections is
-- exactly 1. NOTE: Increasing the number will eventually cause deadlocks!
withDiffDB :: FilePath -> (Pool SQL.Connection -> IO a) -> IO a
withDiffDB fn act = do
  pool <- liftIO $ createPool (SQL.open fn) SQL.close 1 6000 1
  -- migrate / create schema
  withResource pool $ \conn -> do
    SQL.execute_ conn "PRAGMA cache_size = -1000000"; -- use 1G of cache
    runBeamSqlite conn migrateVdiff
    createIndices conn
  x <- act pool
  destroyAllResources pool
  return x


restoreDb :: (HasMainEnv env) => Text -> RIO env ()
restoreDb srcFn = do
  srcDb <- liftIO $ Sqlite3.open srcFn
  pool <- view databaseL
  success <- liftIO $ withResource pool $ \conn -> do
    let dstDb = SQL.connectionHandle conn
    transferDb srcDb dstDb
  liftIO $ Sqlite3.close srcDb
  if success
    then logInfo $ "load from " <> display srcFn <> " into in-memory Db complete"
    else logInfo $ "load from " <> display srcFn <> " into in-memory Db failed"

backupDb :: (HasMainEnv env) => Text -> RIO env ()
backupDb dstFn = do
  dstDb <- liftIO $ Sqlite3.open dstFn
  pool <- view databaseL
  success <- liftIO $ withResource pool $ \conn -> do
    let srcDb = SQL.connectionHandle conn
    transferDb srcDb dstDb
  liftIO $ Sqlite3.close dstDb
  if success
    then logInfo $ "backup to " <> display dstFn <> " complete"
    else logInfo $ "backup to " <> display dstFn <> " failed"


transferDb :: Sqlite3.Database -> Sqlite3.Database -> IO Bool
transferDb srcDb dstDb = do
  bkp <- liftIO $ Sqlite3.backupInit dstDb "main" srcDb "main"
  res <- liftIO $ Sqlite3.backupStep bkp (-1)
  case res of
    Sqlite3.BackupDone -> liftIO (Sqlite3.backupFinish bkp) >> return True
    Sqlite3.BackupOK   -> return False

createIndices :: SQL.Connection -> IO ()
createIndices conn = do
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS `runs_code_hash_idx` ON `runs` (`code_hash`);"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS `runs_verifier_name_idx` ON `runs` (`verifier_name`);"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS `runs_verifier_name_result_idx` ON `runs` (`verifier_name`, `result`);"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS `tmp_counts_run_id_idx` ON `tmp_counts` (`run_id`);"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS `tmp_consensuses_verifier_name_weights_idx` ON `tmp_consensuses` (`code_hash`, `weights`, `verdict`);"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS `tmp_consensuses_xxx_idx` ON `tmp_consensuses` (`code_hash`, `weights`);"
  SQL.execute_ conn "CREATE INDEX IF NOT EXISTS `tmp_consensuses_yyy_idx` ON `tmp_consensuses` (`weights`);"
