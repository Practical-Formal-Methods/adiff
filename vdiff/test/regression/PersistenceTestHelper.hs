module PersistenceTestHelper where

import           Data.Pool
import qualified Database.SQLite.Simple as SQL
import           VDiff.Prelude

withTestEnv :: RIO MainEnv () -> IO ()
withTestEnv act = withSystemTempFile "test.db" $ \fp _ -> do
    pool <- createPool (SQL.open fp) SQL.close 1 60 1
    let logger = NoLogging ^. logFuncL
    runRIO (MainEnv logger pool) act
