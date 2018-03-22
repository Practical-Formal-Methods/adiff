module Types
  ( module Types
  , module Data.Default
  ) where

import           RIO

import           Data.Default
import qualified Database.SQLite.Simple as SQL
import           System.IO              (FilePath)

import           Data
import           Timed


data Strategy = NaiveRandom -- ^ naive random strategy
              | SmartGuided -- ^ not implemented yet


strategyName :: Strategy -> String
strategyName NaiveRandom = "naive"
strategyName SmartGuided = "smart"

--------------------------------------------------------------------------------
-- | * RIO
-- | type classes for usage with RIO instances
class HasDatabase a where
  databaseL :: Lens' a SQL.Connection

class (HasLogFunc a, HasDatabase a) => HasMainEnv a

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
newtype VerifierEnv = VerifierEnv
  { verifierEnvLogger :: LogFunc
  }
instance HasLogFunc VerifierEnv where
  logFuncL = lens verifierEnvLogger (\e l -> e { verifierEnvLogger = l})

mkVerifierEnv :: (HasLogFunc env ) => RIO env VerifierEnv
mkVerifierEnv = do
  lg <- view logFuncL
  return $ VerifierEnv lg

-- TODO: Does this partitioning make sense?
data Conclusion
  = Agreement VerifierResult -- ^ all verifiers agree on an outcome
  | VerifiersUnsound [VerifierName]  -- ^ verifiers that accept the program although the majority does not
  | VerifiersIncomplete [VerifierName] -- ^ verifiers reject the program although the majority does not
  | Disagreement  -- ^ none of the other cases



data Verifier = Verifier
  { verifierName :: VerifierName
  , execute      :: FilePath -> RIO VerifierEnv (VerifierResult, Timing)
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




data DiffParameters = DiffParameters
  { strategy   :: Strategy
  , iterations :: Int
  , verifiers  :: [Verifier]
  , program    :: FilePath
  }

type Property = String
