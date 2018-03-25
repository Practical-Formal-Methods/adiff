module Types
  ( module Types
  , module Data.Default
  ) where

import           RIO

import           Data.Default
import qualified Database.SQLite.Simple as SQL
import           System.IO              (FilePath)

import           Data


data Strategy = NaiveRandom -- ^ naive random strategy
              | SmartGuided -- ^ not implemented yet


strategyName :: Strategy -> String
strategyName NaiveRandom = "naive"
strategyName SmartGuided = "smart"

type Microseconds = Int
--------------------------------------------------------------------------------
-- | * RIO
-- | type classes for usage with RIO instances
class HasDatabase a where
  databaseL :: Lens' a SQL.Connection

class (HasLogFunc a, HasDatabase a) => HasMainEnv a

class HasTimeLimit a where
  timeLimitL :: Lens' a Microseconds

-- | ** The main environment
-- | This is the main environment that is available for all commands.
data MainEnv = MainEnv
  { _logger   :: LogFunc
  , _database :: SQL.Connection
  }
instance HasMainEnv MainEnv

instance HasLogFunc MainEnv where
  logFuncL = lens _logger (\e l -> e {_logger = l})

instance HasDatabase MainEnv where
  databaseL = lens _database (\e d -> e {_database = d})


-- | Every verifier is supposed to run in this environment
data VerifierEnv = VerifierEnv
  { _verifierEnvLogger :: LogFunc
  , _timeLimit         :: Microseconds
  }
instance HasLogFunc VerifierEnv where
  logFuncL = lens _verifierEnvLogger (\e l -> e { _verifierEnvLogger = l})

instance HasTimeLimit VerifierEnv where
  timeLimitL = lens _timeLimit (\e l -> e { _timeLimit = l})

-- | creates a verifier environment given a time limit.
mkVerifierEnv :: (HasLogFunc env ) => Microseconds -> RIO env VerifierEnv
mkVerifierEnv timeLimit = do
  lg <- view logFuncL
  return $ VerifierEnv lg timeLimit

-- TODO: Does this partitioning make sense?
data Conclusion
  = Agreement VerifierResult -- ^ all verifiers agree on an outcome
  | VerifiersUnsound [VerifierName]  -- ^ verifiers that accept the program although the majority does not
  | VerifiersIncomplete [VerifierName] -- ^ verifiers reject the program although the majority does not
  | Disagreement  -- ^ none of the other cases



data Verifier = Verifier
  { verifierName :: VerifierName
  , execute      :: FilePath -> RIO VerifierEnv VerifierResult
  , version      :: IO (Maybe String)
  }

instance Eq Verifier where
  v1 == v2 = verifierName (v1 :: Verifier) == verifierName (v2 :: Verifier)

instance Ord Verifier where
  v1 <= v2 = verifierName (v1 :: Verifier) <= verifierName (v2 :: Verifier)

instance Default Verifier where
  def = Verifier { verifierName = error "verifierName has no default value"
                 , execute  =  const $ return (VerifierTerminated Unknown def)
                 , version = return Nothing
                 }




data DiffParameters = DiffParameters
  { strategy   :: Strategy
  , iterations :: Int
  , verifiers  :: [Verifier]
  , program    :: FilePath
  }

type Property = String
