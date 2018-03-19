{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Types
  ( module Types
  , module Data.Default
  ) where

import           Control.Monad.Trans.Reader
import           Data.Default
import           System.IO                  (FilePath)

import           Timed

data Strategy = NaiveRandom -- ^ naive random strategry
              | SmartGuided -- ^ not implemented yet


strategyName :: Strategy -> String
strategyName NaiveRandom = "naive"
strategyName SmartGuided = "smart"
--------------------------------------------------------------------------------

data VerifierRun = VerifierRun
  { runVerifierName   :: VerifierName
  , verifierResult :: VerifierResult
  , verifierTiming :: Timing
  }


data VerifierResult
  = VerificationSuccessful
  | VerificationFailed
  | VerificationResultUnknown
  deriving (Eq, Show)

-- TODO: Does this partitioning make sense?
data Conclusion
  = Agreement VerifierResult -- ^ all verifiers agree on an outcome
  | VerifiersUnsound [VerifierName]  -- ^ verifiers that accept the program although the majority does not
  | VerifiersIncomplete [VerifierName] -- ^ verifiers reject the program although the majority does not
  | Disagreement  -- ^ none of the other cases

type VerifierName = String

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
  , outputDir  :: Maybe FilePath
  , verifiers  :: [Verifier]
  , program    :: FilePath
  }
  | CmdVersions
  | CmdParseTest FilePath


type Property = String

--------------------------------------------------------------------------------
-- Monad Fun
--------------------------------------------------------------------------------

-- for now it's just a newtype for reader
newtype VDiff a = VDiff {unVDiff :: Reader MainParameters a}

runVdiff :: VDiff a -> MainParameters -> a
runVdiff x = runReader (unVDiff x)

data RunFeedback = RunFeedback -- dummy
--------------------------------------------------------------------------------
