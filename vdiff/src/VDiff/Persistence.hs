module VDiff.Persistence where

import           VDiff.Prelude


import           Database.Beam
import qualified Database.Beam.Sqlite   as Sqlite
import qualified Database.SQLite.Simple as SQL
import           VDiff.Data
-- import           VDiff.DataOld

--------------------------------------------------------------------------------
-- * Persistence
--------------------------------------------------------------------------------

-- | initializes the database with the necessary parameters. If not file path is given, the default name "vdiff.db" is used.
withDiffDB :: FilePath -> (SQL.Connection -> IO a) -> IO a
withDiffDB fn act = do
  conn <- liftIO $ SQL.open fn
  -- liftIO $ SQL.execute_ conn "CREATE TABLE IF NOT EXISTS programs (code_hash TEXT PRIMARY KEY, origin TEXT, content TEXT)"
  -- liftIO $ SQL.execute_ conn "CREATE TABLE IF NOT EXISTS runs (run_id INTEGER PRIMARY KEY, verifier_name TEXT, result TEXT, time FLOAT, memory INT, code_hash TEXT REFERENCES programs )"
  x <- act conn
  SQL.close conn
  return x


runBeam :: (HasDatabase env) => Sqlite.SqliteM a -> RIO env a
runBeam act = do
  env <- ask
  liftIO $ Sqlite.runBeamSqlite (env ^. databaseL) act

query_ :: (SQL.FromRow r, HasDatabase env) => SQL.Query -> RIO env [r]
query_ q = do
  conn <- view databaseL
  liftIO $ SQL.query_ conn q

query :: (HasDatabase env, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> RIO env [r]
query q params = do
  conn <- view databaseL
  liftIO $ SQL.query conn q params

execute_ :: ( MonadReader env m, HasDatabase env, MonadIO m) => SQL.Query -> m ()
execute_ q = do
  conn <- view databaseL
  liftIO $ SQL.execute_ conn q

fold_ :: (HasDatabase env, SQL.FromRow row) => SQL.Query -> a -> (a -> row -> IO a) -> RIO env a
fold_ q z f = do
  conn <- view databaseL
  liftIO $ SQL.fold_ conn q z f



storeProgram :: (HasDatabase env) => Program -> RIO env ()
storeProgram p = runBeam $ runInsert $ insert (vdiffDb ^. programs) $ insertValues [p]

storeRun :: (HasDatabase env) => VerifierRun -> RIO env ()
storeRun r = runBeam $ runInsert $ insert (vdiffDb ^. runs) $ insertValues [r]


