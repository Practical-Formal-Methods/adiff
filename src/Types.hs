{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Types where

import           Control.Monad.Trans.Reader

import           System.IO                  (FilePath)

data Strategy = NaiveRandom -- ^ naive random strategry
              | SmartGuided -- ^ not implemented yet


strategyName :: Strategy -> String
strategyName NaiveRandom = "naive"
strategyName SmartGuided = "smart"
--------------------------------------------------------------------------------





data MainParameters = MainParameters
  { verbose  :: Bool
  , strategy :: Strategy
  , program  :: FilePath
  }

--------------------------------------------------------------------------------
-- Monad Fun
--------------------------------------------------------------------------------

-- for now it's just a newtype for reader
newtype VDiff a = VDiff {unVDiff :: Reader MainParameters a}

runVdiff :: VDiff a -> MainParameters -> a
runVdiff x = runReader (unVDiff x)

data RunFeedback = RunFeedback -- dummy
--------------------------------------------------------------------------------
