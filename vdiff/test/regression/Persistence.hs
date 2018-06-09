module Persistence (testPersistence) where

import           RIO
import           Test.Tasty

import           VDiff.Data

import           Database.Beam
import           Database.Beam.Migrate.Simple
import qualified Database.Beam.Sqlite         as Sqlite
import           Database.Beam.Sqlite.Migrate (migrationBackend)
import           Database.SQLite.Simple
import           System.IO

import           Test.Tasty.HUnit

testPersistence :: TestTree
testPersistence = testGroup "persistence" [testSimpl]

testSimpl :: TestTree
testSimpl = testCase "simple interactions" $
  withSystemTempFile "test.db" $ \fp _ -> do
    conn <- open fp
    Sqlite.runBeamSqliteDebug putStrLn conn $ do
      -- start migration
      autoMigrate migrationBackend vdiffDbChecked
      let somePrograms = [ Program "ff" "test.c" "int main(){}"
                         , Program "ef" "bla.i" ""
                         ]
      -- insert some programs
      runInsert $ insert (vdiffDb ^. programs) $
        insertValues somePrograms
      -- and now we query those programs
      somePrograms' <- runSelectReturningList $ select $ all_ (vdiffDb ^. programs)
      liftIO (somePrograms @=? somePrograms')
