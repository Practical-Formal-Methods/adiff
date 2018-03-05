{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Types
  ( module Types
  , module Data.Default
  ) where

import           Control.Monad.Trans.Reader

import           Data.Default
import           System.IO                  (FilePath)

data Strategy = NaiveRandom -- ^ naive random strategry
              | SmartGuided -- ^ not implemented yet


strategyName :: Strategy -> String
strategyName NaiveRandom = "naive"
strategyName SmartGuided = "smart"
--------------------------------------------------------------------------------


-- | TODO: Add execution time and more
data VerifierResult = VerificationSuccessful | VerificationFailed | VerificationResultUnknown
  deriving (Show, Eq)

data Verifier = Verifier
  { verifierName :: String
  , execute      :: FilePath -> IO VerifierResult
  , version      :: IO (Maybe String)
  }

instance Default Verifier where
  def = Verifier { verifierName = error "verifierName has no default value"
                 , execute  =  const (return VerificationResultUnknown)
                 , version = return Nothing
                 }



data MainParameters = CmdRun
  { verbose   :: Bool
  , strategy  :: Strategy
  , verifiers :: [Verifier]
  , program   :: FilePath
  }
  | CmdVersions

--------------------------------------------------------------------------------
-- Monad Fun
--------------------------------------------------------------------------------

-- for now it's just a newtype for reader
newtype VDiff a = VDiff {unVDiff :: Reader MainParameters a}

runVdiff :: VDiff a -> MainParameters -> a
runVdiff x = runReader (unVDiff x)

data RunFeedback = RunFeedback -- dummy
--------------------------------------------------------------------------------
