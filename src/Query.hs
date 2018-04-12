{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | support queries to the database
module Query where

import           RIO

import           Control.Lens.TH
import           Data.FileEmbed
import           Data.List                 (isInfixOf)
import           Data.Text.Encoding
import           Database.SQLite.Simple    (field)
import qualified Database.SQLite.Simple    as SQL
import           Safe
import qualified Text.PrettyPrint.Tabulate as T

import           Data
import           Persistence
import           Types

data Query = Incomplete | Unsound
  deriving (Show, Eq)

type Statistics = [(Text, Text)]

stats :: (HasDatabase env) => RIO env Statistics
stats = do
  [runsN :: SQL.Only Int] <- query_ "SELECT COUNT(*) FROM runs;"
  [programsN :: SQL.Only Int] <- query_ "SELECT COUNT(*) FROM programs;"
  [distinctOrigin :: SQL.Only Int] <- query_ "SELECT COUNT(DISTINCT origin) frOM programs;"
  return [ ("runs", tshow $ SQL.fromOnly runsN)
         , ("programs", tshow $ SQL.fromOnly programsN)
         , ("used source files", tshow $ SQL.fromOnly distinctOrigin)
         ]

data RunFinding = RunFinding
  { runId         :: Int
  , _verifierName :: String
  , _originalFn   :: String
  , _programHash  :: String
  , _unsats       :: Int
  , _sats         :: Int
  } deriving (Data, Generic)

makeFieldsNoPrefix ''RunFinding

instance SQL.FromRow RunFinding where
  fromRow = RunFinding <$> field <*> field <*> field <*> field <*> field <*> field

instance SQL.FromRow CProgram where
  fromRow = do
    h <- field
    o <- field
    c <- field
    return $ CProgram c o h


allIncomplete :: (HasDatabase env) => RIO env [RunFinding]
allIncomplete = query_ $ SQL.Query (decodeUtf8 $(embedFile "assets/sql/incompleteness.sql"))

allUnsound :: (HasDatabase env) => RIO env [RunFinding]
allUnsound = query_ $ SQL.Query (decodeUtf8 $(embedFile "assets/sql/unsoundness.sql"))


-- this folds over the complete database because sqlite does not have string matching
programByHash :: (HasDatabase env) => String -> RIO env (Maybe CProgram)
programByHash hsh = do
  let regex = "'" ++ hsh ++ "*'"
  -- (prgs :: [CProgram]) <- query "SELECT * FROM programs WHERE code_hash MATCH ?" [regex]
  prgs <- fold_ "SELECT * FROM programs" [] f
  return $ headMay prgs
  where
    f ls prg = if (hsh `isInfixOf` (show $ prg ^. hash))
               then return (prg:ls)
               else return ls



