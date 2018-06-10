{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module VDiff.Query.Types where

import           Control.Lens
import           RIO

import           VDiff.Data

data RunFinding = RunFinding
  { _run    :: VerifierRun
  , _unsats :: Int
  , _sats   :: Int
  } deriving (Show, Generic)

makeFieldsNoPrefix ''RunFinding
