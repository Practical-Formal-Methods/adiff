module Persistence (testPersistence) where

import           PersistenceTestHelper
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Prelude                      as P
import           VDiff.Data

import           Database.Beam
import           Database.Beam.Migrate.Simple
import           Database.Beam.Sqlite.Migrate (migrationBackend)

import           VDiff.Persistence
import           VDiff.Prelude
import qualified VDiff.Query2                 as Q2


testPersistence :: TestTree
testPersistence = testGroup "persistence" [ testSchemaCreation
                                          -- , testMigration
                                          ]



testSchemaCreation :: TestTree
testSchemaCreation = testCase "schema creation" $ withTestEnv $ do
  -- start migration
  runBeam $ autoMigrate migrationBackend vdiffDbChecked
  let somePrograms = [ Program "ff" "test.c" "int main(){}"
                      , Program "ef" "bla.i" ""
                      ]
  -- insert some programs
  mapM_ Q2.storeProgram somePrograms
  -- inserting the first program twice, shouldn't change anything
  Q2.storeProgram (P.head somePrograms)

  -- and now we query those programs
  somePrograms' <- runBeam $ runSelectReturningList $ select $ all_ (vdiffDb ^. programs)
  liftIO (somePrograms @=? somePrograms')

-- testMigration :: TestTree
-- testMigration = testCase "test migration from 'old' schema" $ withTestEnv $ do
--   -- re-create the old schema
--   conn <- view databaseL
--   liftIO $ SQL.execute_ conn "CREATE TABLE IF NOT EXISTS programs (code_hash TEXT PRIMARY KEY, origin TEXT, content TEXT)"
--   liftIO $ SQL.execute_ conn "CREATE TABLE IF NOT EXISTS runs (run_id INTEGER PRIMARY KEY, verifier_name TEXT, result TEXT, time FLOAT, memory INT, code_hash TEXT REFERENCES programs )"

--   -- now start the automatic migration
--   runBeam migrateVdiff
