-- MIT License
--
-- Copyright (c) 2018 Christian Klinger
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Persistence (testPersistence) where

import           PersistenceTestHelper
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Prelude                      as P
import           ADiff.Data

import           Database.Beam
import           Database.Beam.Migrate.Simple
import           Database.Beam.Sqlite.Migrate (migrationBackend)

import           ADiff.Persistence
import           ADiff.Prelude
import qualified ADiff.Query2                 as Q2


testPersistence :: TestTree
testPersistence = testGroup "persistence" [ testSchemaCreation
                                          -- , testMigration
                                          ]



testSchemaCreation :: TestTree
testSchemaCreation = testCase "schema creation" $ withTestEnv $ do
  -- start migration
  runBeam $ autoMigrate migrationBackend adiffDbChecked
  let somePrograms = [ Program "ff" "test.c" "int main(){}"
                      , Program "ef" "bla.i" ""
                      ]
  -- insert some programs
  mapM_ Q2.storeProgram somePrograms
  -- inserting the first program twice, shouldn't change anything
  Q2.storeProgram (P.head somePrograms)

  -- and now we query those programs
  somePrograms' <- runBeam $ runSelectReturningList $ select $ all_ (adiffDb ^. programs)
  liftIO (somePrograms @=? somePrograms')

-- testMigration :: TestTree
-- testMigration = testCase "test migration from 'old' schema" $ withTestEnv $ do
--   -- re-create the old schema
--   conn <- view databaseL
--   liftIO $ SQL.execute_ conn "CREATE TABLE IF NOT EXISTS programs (code_hash TEXT PRIMARY KEY, origin TEXT, content TEXT)"
--   liftIO $ SQL.execute_ conn "CREATE TABLE IF NOT EXISTS runs (run_id INTEGER PRIMARY KEY, verifier_name TEXT, result TEXT, time FLOAT, memory INT, code_hash TEXT REFERENCES programs )"

--   -- now start the automatic migration
--   runBeam migrateVdiff
