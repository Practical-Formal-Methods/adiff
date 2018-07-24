module VDiff.Persistence where

import           VDiff.Prelude

import qualified Crypto.Hash.SHA1               as SHA1
import qualified Data.ByteString.Base16         as Hex
import qualified Data.ByteString.Char8          as C8
import Data.Pool

import           Database.Beam
import           Database.Beam.Sqlite           as Sqlite
import qualified Database.SQLite.Simple         as SQL
import qualified Data.Text as T
import           Database.SQLite.Simple.ToField
import           System.IO                      (putStrLn)

import           VDiff.Data
import           VDiff.Timed
--------------------------------------------------------------------------------
-- * Persistence
--------------------------------------------------------------------------------


runBeam :: (HasDatabase env, HasLogFunc env) => Sqlite.SqliteM a -> RIO env a
runBeam act = do
  env <- ask
  logger <- view logFuncL
  let lf = runRIO env . logDebug . display . T.pack
  let pool = env ^. databaseL
  liftIO $ withResource pool $ \conn -> retryOnBusy $ Sqlite.runBeamSqliteDebug lf  conn act

retryOnBusy = retryOnBusy' 0

retryOnBusy' :: Int -> IO a -> IO a
retryOnBusy' i action = catch action $ \(e :: SQL.SQLError) ->
  case e of
    SQL.SQLError SQL.ErrorBusy _ _ -> do
      let wait = round (1.2^i)
      putStrLn $ "busy, waiting " ++ show wait  ++ " s"
      threadDelay (wait * 1000 * 1000)
      retryOnBusy' (i+1) action
    _ -> throwIO e

query_ :: (SQL.FromRow r, HasDatabase env) => SQL.Query -> RIO env [r]
query_ q = do
  pool <- view databaseL
  liftIO $ withResource pool $ \conn -> SQL.query_ conn q

query :: (HasDatabase env, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> RIO env [r]
query q params = do
  pool <- view databaseL
  liftIO $ withResource pool $ \conn -> SQL.query conn q params

execute_ :: ( MonadReader env m, HasDatabase env, MonadIO m) => SQL.Query -> m ()
execute_ q = do
  pool <- view databaseL
  liftIO $ withResource pool $ \conn -> SQL.execute_ conn q

fold_ :: (HasDatabase env, SQL.FromRow row) => SQL.Query -> a -> (a -> row -> IO a) -> RIO env a
fold_ q z f = do
  pool <- view databaseL
  liftIO $ withResource pool $ \conn -> SQL.fold_ conn q z f

