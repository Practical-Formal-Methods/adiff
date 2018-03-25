module Persistence where

import           RIO

import qualified Crypto.Hash.SHA1               as SHA1
import qualified Data.ByteString.Base16         as Hex
import qualified Data.ByteString.Char8          as C8

import qualified Database.SQLite.Simple         as SQL
import           Database.SQLite.Simple.ToField

import           Data
import           Timed
--------------------------------------------------------------------------------
-- * Persistence
--------------------------------------------------------------------------------

-- | initializes the database with the necessary parameters. If not file path is given, the default name "vdiff.db" is used.
withDiffDB :: FilePath -> (SQL.Connection -> IO a) -> IO a
withDiffDB fn act = do
  conn <- liftIO $ SQL.open fn
  liftIO $ SQL.execute_ conn "CREATE TABLE IF NOT EXISTS programs (code_hash TEXT PRIMARY KEY, origin TEXT, content TEXT)"
  liftIO $ SQL.execute_ conn "CREATE TABLE IF NOT EXISTS runs (run_id INTEGER PRIMARY KEY, verifier_name TEXT, result TEXT, time INT, memory INT, code_hash TEXT REFERENCES programs )"
  x <- act conn
  SQL.close conn
  return x


-- | For things that should be stored into the database
class Persistent a where
  persist :: SQL.Connection -> a -> IO ()

instance Persistent CProgram where
  persist conn p = SQL.execute conn "INSERT INTO programs(code_hash, origin, content) VALUES(?,?,?) " row
    where row = [ toField (hash (programSource p))
                , toField (programOriginalFilename p)
                , toField (programSource p)
                ]

instance Persistent VerifierRun where
  persist conn run = do
    let row = [ toField (runVerifierName run)
              , toField (verdict $ verifierResult run)
              , maybe SQL.SQLNull (toField.elapsedWall)  (timing $ verifierResult run)
              , maybe SQL.SQLNull (toField.maxResidentMemory) (timing $ verifierResult run)
              , toField (verifierCode run)
              ]
    SQL.execute conn "INSERT INTO runs(verifier_name,result,time,memory,code_hash) VALUES (?,?,?,?,?)" row


--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

hash :: String -> Hashed a
hash =  Hashed . Hex.encode . SHA1.hash . C8.pack
