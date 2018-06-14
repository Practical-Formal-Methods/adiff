{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- This module redefines the currently used data structure with the help of 'beam'
module VDiff.Data (
  -- * Programs
    Program
  , ProgramT(Program)
  , ProgramId
  , mkProgram
  , hash
  , origin
  , source
  -- * Results
  , Verdict(..)
  , VerifierResult
  , VerifierResultMixin(VerifierResult)
  , memory
  , wallTime
  , verdict
  -- * Runs
  , VerifierRunT(VerifierRun)
  , VerifierRun
  , VerifierRunId
  , program
  , result
  , runId
  , verifierName
  -- * database
  , runs
  , programs
  -- * Table configuration
  , vdiffDb
  , vdiffDbChecked
  , VDiffDb
  -- * Others
  , VerifierName
  , default_
  ) where

import           RIO

import           Control.Lens
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Connection
import           Database.SQLite.Simple.FromField
import qualified Crypto.Hash.SHA1                   as SHA1
import qualified Data.ByteString.Base16             as Hex
import qualified Data.ByteString.Char8              as C8

type VerifierName = Text

-- | An instrumented program
data ProgramT f = Program
  { _hash   :: C f Text
  , _origin :: C f Text
  , _source :: C f Text
  } deriving (Generic, Beamable)

Program (LensFor hash) (LensFor origin) (LensFor source) = tableLenses


mkProgram :: FilePath -> String -> Program
mkProgram origin content = Program (mkHashS content) (T.pack origin) (T.pack content)
  where
    mkHashS :: String -> Text
    mkHashS =  T.decodeUtf8 . Hex.encode . SHA1.hash . C8.pack

type Program = ProgramT Identity
type ProgramId = PrimaryKey ProgramT Identity

deriving instance Show Program
deriving instance Eq Program

--------------------------------------------------------------------------------

data Verdict = Sat | Unsat | Unknown
      deriving (Show, Read, Eq, Ord, Enum)

instance (IsSql92DataTypeSyntax s) => HasDefaultSqlDataType s Verdict where
  defaultSqlDataType _ _ = varCharType Nothing Nothing

instance (IsSql92ColumnSchemaSyntax s) => HasDefaultSqlDataTypeConstraints s Verdict

instance (IsSql92ExpressionSyntax s) => HasSqlEqualityCheck s Verdict

instance (HasSqlValueSyntax s Text) => HasSqlValueSyntax s Verdict where
  sqlValueSyntax = sqlValueSyntax . verdictToText
    where
      verdictToText :: Verdict -> Text
      verdictToText Sat = "sat"
      verdictToText Unsat = "unsat"
      verdictToText Unknown = "unknown"

instance FromField Verdict where
  fromField f =  do
    (t :: Text) <- fromField f
    case t of
      "sat" -> return Sat
      "unsat" -> return Unsat
      "unknown" -> return Unknown
      _ -> fail $ "unrecognized enum element: " ++ (T.unpack t)

instance FromBackendRow (Database.Beam.Sqlite.Connection.Sqlite) Verdict

--------------------------------------------------------------------------------

data VerifierResultMixin f = VerifierResult
  { _wallTime :: C f (Maybe Double)
  , _memory   :: C f (Maybe Int)
  , _verdict  :: C f Verdict
  } deriving (Generic, Beamable)

VerifierResult (LensFor wallTime) (LensFor memory) (LensFor verdict) = tableLenses
-- makeFieldsNoPrefix ''VerifierResultMixin

type VerifierResult = VerifierResultMixin Identity

deriving instance Show VerifierResult


instance Table ProgramT where
  data PrimaryKey ProgramT f = ProgramId (C f Text) deriving (Generic, Beamable)
  primaryKey = ProgramId . _hash

-- | A run of one verifier on one program
data  VerifierRunT f = VerifierRun
  { _runId        :: C f Int
  , _verifierName :: C f Text
  , _program      :: PrimaryKey ProgramT f
  , _result       :: VerifierResultMixin f
  } deriving (Generic, Beamable)
makeFieldsNoPrefix ''VerifierRunT

type VerifierRun = VerifierRunT Identity
type VerifierRunId = PrimaryKey VerifierRunT Identity

instance Table VerifierRunT where
  data PrimaryKey VerifierRunT f = VerifierRunId (C f Int) deriving (Generic, Beamable)
  primaryKey = VerifierRunId . _runId

deriving instance Show (PrimaryKey ProgramT Identity)
-- deriving instance Eq (PrimaryKey ProgramT Identity)
deriving instance Show VerifierRun
-- deriving instance Eq VerifierRun


--- and now we define the database
data VDiffDb f = VDiffDb
  { _runs     :: f (TableEntity VerifierRunT)
  , _programs :: f (TableEntity ProgramT)
  } deriving Generic

VDiffDb (TableLens runs) (TableLens programs) = dbLenses

instance Database be VDiffDb


vdiffDbChecked :: CheckedDatabaseSettings be VDiffDb
vdiffDbChecked = defaultMigratableDbSettings @SqliteCommandSyntax `withDbModification` modification
  where
    modification = dbModification
      { _runs     = modifyCheckedTable (const "runs") mod_runs
      , _programs = modifyCheckedTable (const "programs") mod_programs
      }
    mod_runs = checkedTableModification
      { _runId        = "run_id"
      , _verifierName = "verifier_name"
      , _program      = ProgramId "code_hash"
      , _result       = VerifierResult "time" "memory" "result"
      }
    mod_programs = checkedTableModification
      { _hash   = "code_hash"
      , _origin = "origin"
      , _source = "source"
      }

vdiffDb :: DatabaseSettings be VDiffDb
vdiffDb = unCheckDatabase vdiffDbChecked
