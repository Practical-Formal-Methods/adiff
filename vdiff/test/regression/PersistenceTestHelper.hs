module PersistenceTestHelper where

import qualified Database.SQLite.Simple as SQL
import           VDiff.Prelude

withTestEnv :: RIO MainEnv () -> IO ()
withTestEnv act = withSystemTempFile "test.db" $ \fp _ -> do
    conn <- SQL.open fp
    let logger = NoLogging ^. logFuncL
    runRIO (MainEnv logger conn) act
