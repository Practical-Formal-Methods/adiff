{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Types
  ( module Types
  , module Data.Default
  ) where

import           Control.Monad.Trans.Reader
import           Data.ByteString
import qualified Data.ByteString.Base16         as Hex
import qualified Data.ByteString.Char8          as C8
import           Data.Default
import           System.IO                      (FilePath)


import qualified Database.SQLite.Simple         as SQL
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.ToRow

import           Data
import           Timed

data Strategy = NaiveRandom -- ^ naive random strategy
              | SmartGuided -- ^ not implemented yet


strategyName :: Strategy -> String
strategyName NaiveRandom = "naive"
strategyName SmartGuided = "smart"


-- data VerifierRuns = VerifierRuns
--   { verifierRuns :: [VerifierRun]
--   , code         :: Hashed String
--   } deriving (Show)




-- TODO: Does this partitioning make sense?
data Conclusion
  = Agreement VerifierResult -- ^ all verifiers agree on an outcome
  | VerifiersUnsound [VerifierName]  -- ^ verifiers that accept the program although the majority does not
  | VerifiersIncomplete [VerifierName] -- ^ verifiers reject the program although the majority does not
  | Disagreement  -- ^ none of the other cases


data Verifier = Verifier
  { verifierName :: VerifierName
  , execute      :: FilePath -> IO (VerifierResult, Timing)
  , version      :: IO (Maybe String)
  }

instance Eq Verifier where
  v1 == v2 = verifierName (v1 :: Verifier) == verifierName (v2 :: Verifier)

instance Ord Verifier where
  v1 <= v2 = verifierName (v1 :: Verifier) <= verifierName (v2 :: Verifier)

instance Default Verifier where
  def = Verifier { verifierName = error "verifierName has no default value"
                 , execute  =  const $ return (VerificationResultUnknown, def)
                 , version = return Nothing
                 }



data MainParameters = CmdRun
  { verbose    :: Bool
  , strategy   :: Strategy
  , iterations :: Int
  , databaseFn :: Maybe FilePath
  , verifiers  :: [Verifier]
  , program    :: FilePath
  }
  | CmdVersions
  | CmdParseTest FilePath


type Property = String
