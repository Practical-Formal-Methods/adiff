module VDiff.Persistence where

import           VDiff.Prelude

import qualified Crypto.Hash.SHA1               as SHA1
import qualified Data.ByteString.Base16         as Hex
import qualified Data.ByteString.Char8          as C8

import           Database.Beam
import           Database.Beam.Sqlite           as Sqlite
import qualified Database.SQLite.Simple         as SQL
import           Database.SQLite.Simple.ToField
import           System.IO                      (putStrLn)

import           VDiff.Data
import           VDiff.Timed
--------------------------------------------------------------------------------
-- * Persistence
--------------------------------------------------------------------------------


runBeam :: (HasDatabase env) => Sqlite.SqliteM a -> RIO env a
runBeam act = do
  env <- ask
  liftIO $ Sqlite.runBeamSqlite (env ^. databaseL) act

runBeamDebug :: (HasDatabase env) => Sqlite.SqliteM a -> RIO env a
runBeamDebug act = do
  env <- ask
  liftIO $ Sqlite.runBeamSqliteDebug putStrLn (env ^. databaseL) act



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

