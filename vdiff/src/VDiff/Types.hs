{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- TODO: better name: Prelude

module VDiff.Types
  ( module VDiff.Types
  , module Data.Default
  , module Language.C
  , module Language.C.Data.Lens
  , module Language.C.Analysis.AstAnalysis2
  , Type
  , makeFieldsNoPrefix
  ) where

import           RIO

import           Control.Lens.TH
import           Control.Monad.Random
import           Data.Default
import           Data.List                        (intersperse)
import           Data.Text                        (pack)
import qualified Database.SQLite.Simple           as SQL
import           Language.C                       hiding (LevelError, LevelWarn,
                                                   execParser)
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.SemRep       hiding (Stmt)
import           Language.C.Data.Lens
import           System.IO                        (FilePath)
import           Text.PrettyPrint                 (render)

import           VDiff.Data


data Strategy = RandomWalkStrategy
              | RandomUniformStrategy
              | BreadthFirstStrategy
              | DepthFirstStrategy
              | SmartStrategy

type Microseconds = Int
--------------------------------------------------------------------------------
-- | * RIO
-- | type classes for usage with RIO instances
class HasDatabase a where
  databaseL :: Lens' a SQL.Connection

class (HasLogFunc a, HasDatabase a) => HasMainEnv a

class HasTimeLimit a where
  timeLimitL :: Lens' a Microseconds

class HasDiffParameters env where
  diffParameters :: Lens' env DiffParameters

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
  { _verifierEnvLogger :: !LogFunc
  , _timeLimit         :: !Microseconds
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

class HasTranslationUnit env where
  translationUnit :: Lens' env (CTranslationUnit SemPhase)

data StrategyEnv = StrategyEnv
  { _strategyLogFunc         :: !LogFunc
  , _strategyTranslationUnit :: !(CTranslationUnit SemPhase)
  , _strategyDiffParameters  :: !DiffParameters
  , _strategyDatabase        :: !SQL.Connection
  }

instance HasTranslationUnit StrategyEnv where
  translationUnit = lens _strategyTranslationUnit (\e t -> e {_strategyTranslationUnit = t})

instance HasLogFunc StrategyEnv where
  logFuncL = lens _strategyLogFunc (\e f -> e {_strategyLogFunc = f})

instance HasDiffParameters StrategyEnv where
  diffParameters = lens _strategyDiffParameters (\e p -> e {_strategyDiffParameters = p})

instance HasDatabase StrategyEnv where
  databaseL = lens _strategyDatabase (\e d -> e {_strategyDatabase = d})

instance IsStrategyEnv StrategyEnv

class (HasDiffParameters env, HasTranslationUnit env, HasLogFunc env, HasDatabase env) => IsStrategyEnv env


mkStrategyEnv :: (HasMainEnv env) => CTranslationUnit SemPhase -> DiffParameters -> RIO env StrategyEnv
mkStrategyEnv tu dp = do
  lg <- view logFuncL
  db <- view databaseL
  return $ StrategyEnv  lg tu dp db

data Conclusion
  = StrongAgreement !Verdict       -- ^ all verifiers agree on an outcome
  | WeakAgreement   !Verdict       -- ^ all verifiers that terminate with sat or unsat agree
  | Unsoundness    ![VerifierName] -- ^ verifiers that accept the program although the majority does not
  | Incompleteness ![VerifierName] -- ^ verifiers reject the program although the majority does not
  | Disagreement                  -- ^ none of the other cases
  deriving (Show)



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
                 , execute  =  error "verifierExecute has no default value"
                 , version = return Nothing
                 }




data DiffParameters = DiffParameters
  { _strategy  :: Strategy
  , _budget    :: Int
  , _timelimit :: Int
  , _verifiers :: [Verifier]
  , _program   :: FilePath
  }

makeFieldsNoPrefix ''DiffParameters

type Property = String
type TU       = CTranslationUnit SemPhase
type Stmt     = CStatement SemPhase


displayList :: Display a => [a] -> Utf8Builder
displayList xs = mconcat $ intersperse ", " (map display xs)

instance Display Stmt where
  display = display . pack . prettyp


prettyp :: Pretty a => a -> String
prettyp = render . pretty

deriving instance MonadRandom (RIO env)
