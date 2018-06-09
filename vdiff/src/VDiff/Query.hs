{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | support queries to the database
module VDiff.Query where

import           VDiff.Prelude              hiding (Disagreement)

import           Control.Lens.TH
import           Data.List                  (isInfixOf)
import           Database.SQLite.Simple     (field)
import qualified Database.SQLite.Simple     as SQL
import           Safe
import           Text.PrettyPrint.Tabulate  ()

import           VDiff.Data
import           VDiff.Persistence
import           VDiff.Persistence.Internal

data Query
  = Incomplete
  | Unsound
  | Disagreement
  | UnsoundAccordingToKleeOrCbmc
  | UnsoundAccordingToKleeOrCbmcOrSmack
  | Everything
  deriving (Show, Eq)

type Statistics = [(Text, Text)]

queryFromName :: String -> Maybe Query
queryFromName "incomplete"              = Just Incomplete
queryFromName "unsound"                 = Just Unsound
queryFromName "disagreement"            = Just Disagreement
queryFromName "unsound-klee-cbmc"       = Just UnsoundAccordingToKleeOrCbmc
queryFromName "unsound-klee-cbmc-smack" = Just UnsoundAccordingToKleeOrCbmcOrSmack
queryFromName "everything"              = Just Everything
queryFromName _                         = Nothing

stats :: (HasDatabase env) => RIO env Statistics
stats = undefined

-- stats :: (HasDatabase env) => RIO env Statistics
-- stats = do
--   [runsN :: SQL.Only Int] <- query_ "SELECT COUNT(*) FROM runs;"
--   [programsN :: SQL.Only Int] <- query_ "SELECT COUNT(*) FROM programs;"
--   [distinctOrigin :: SQL.Only Int] <- query_ "SELECT COUNT(DISTINCT origin) FROM programs;"
--   return [ ("runs", tshow $ SQL.fromOnly runsN)
--          , ("programs", tshow $ SQL.fromOnly programsN)
--          , ("used source files", tshow $ SQL.fromOnly distinctOrigin)
--          ]

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



allIncomplete :: (HasDatabase env) => RIO env [RunFinding]
allIncomplete = query_ $(embedQuery "incompleteness.sql")

allUnsound :: (HasDatabase env) => RIO env [RunFinding]
allUnsound = query_ $(embedQuery "unsoundness.sql")

allDisagreement :: (HasDatabase env) => RIO env [RunFinding]
allDisagreement = query_ $(embedQuery "disagreement.sql")

-- | When Klee or CBMC says "sat" they are most likely right. In those cases
-- it's very likely that a disagreeing verifier is unsound, hence especially interesting to us.
allUnsoundAccordingToKleeOrCbmc :: (HasDatabase env) => RIO env [RunFinding]
allUnsoundAccordingToKleeOrCbmc = query_ $(embedQuery "unsound-klee-cbmc.sql")


allUnsoundAccordingToKleeOrCbmcOrSmack :: (HasDatabase env) => RIO env [RunFinding]
allUnsoundAccordingToKleeOrCbmcOrSmack = query_ $(embedQuery "unsound-klee-cbmc-smack.sql")


allRuns :: (HasDatabase env) => RIO env [(String, String, Maybe Double, Maybe Int)]
allRuns = query_ "SELECT verifier_name,result,time,memory FROM runs;"

allRuns' :: (HasDatabase env) => RIO env [RunFinding]
allRuns' = query_ $(embedQuery "all-runs.sql")

allRunsByHash :: (HasDatabase env) => String -> RIO env [(String, String, Maybe Double, Maybe Int)]
allRunsByHash str = query "SELECT verifier_name,result,time,memory FROM RUNS WHERE code_hash = ? " (SQL.Only str)


programByHash :: (HasDatabase env) => String -> RIO env (Maybe Program)
programByHash = undefined
-- this folds over the complete database because sqlite does not have string matching
-- programByHash :: (HasDatabase env) => String -> RIO env (Maybe Program)
-- programByHash hsh = do
--   prgs <- fold_ "SELECT * FROM programs" [] f
--   return $ headMay prgs
--   where
--     f ls prg = if hsh `isInfixOf` show (prg ^. hash)
--                then return (prg:ls)
--                else return ls


updateIndices :: (HasDatabase env, HasLogFunc env) => RIO env ()
updateIndices = do
  -- logDebug "updating indices"
  execute_ $(embedQuery "update-indices.sql")

executeQuery q = case q of
  Incomplete                          -> allIncomplete
  Unsound                             -> allUnsound
  Disagreement                        -> allDisagreement
  UnsoundAccordingToKleeOrCbmc        -> allUnsoundAccordingToKleeOrCbmc
  UnsoundAccordingToKleeOrCbmcOrSmack -> allUnsoundAccordingToKleeOrCbmcOrSmack
  Everything                          -> allRuns'
