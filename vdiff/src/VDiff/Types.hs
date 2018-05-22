{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


-- TODO: better name: Prelude

module VDiff.Types
  ( module VDiff.Types
  , module Language.C
  , module Language.C.Data.Lens
  , module Language.C.Analysis.AstAnalysis2
  , Type
  , makeFieldsNoPrefix
  , nub
  , MonadRandom(..)
  ) where

import           RIO

import           Control.Lens.TH
import           Control.Monad.Random
import           Data.List                        (intersperse, (!!))
import           Data.List.Key                    (nub)
import           Data.Text                        (pack)
import qualified Database.SQLite.Simple           as SQL
import           Language.C                       hiding (LevelError, LevelWarn,
                                                   execParser)
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.SemRep       hiding (Stmt)
import           Language.C.Data.Lens
import           Safe
import           System.IO                        (FilePath)
import           Text.PrettyPrint                 (render)

import           VDiff.Data


data Strategy = RandomWalkStrategy
              | RandomUniformStrategy
              | BreadthFirstStrategy
              | DepthFirstStrategy
              | SmartStrategy

data SearchMode = IdentOnly | Subexpressions
  deriving Show

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




data DiffParameters = DiffParameters
  { _strategy   :: Strategy
  , _budget     :: Int
  , _timelimit  :: Int
  , _verifiers  :: [Verifier]
  , _searchMode :: SearchMode
  , _program    :: FilePath
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


--------------------------------------------------------------------------------
-- Utilities regarding random decisions

deriving instance MonadRandom (RIO env)

chooseOneOf :: (MonadRandom m) => [a] ->  m (Maybe a)
chooseOneOf options = do
  i <- getRandomR (0, length options - 1)
  return (options `atMay` i)

randomlyBranch :: (MonadRandom m) => [m a] -> m a
randomlyBranch xs = chooseOneOf xs >>= \case
  Nothing -> error "randomlyBranch needs at least one branch"
  Just act -> act

-- | Similar to 'randomlyBranch', but here a branch can fail (by returning
-- @Nothing@) in which another branch is randomly chosen until one branch succeeds
-- or there are no remaining branches
randomlyBranchMay :: (MonadRandom m) => [m (Maybe a)] -> m (Maybe a)
randomlyBranchMay [] =  return Nothing
randomlyBranchMay l = do
  idx <- getRandomR (0, length l - 1)
  (l !! idx) >>= \case
    Nothing -> randomlyBranchMay (deleteIndex idx l)
    Just r  -> return $ Just r

-- | Partial function
deleteIndex 0 (x:xs) = xs
deleteIndex n (x:xs) = x : (deleteIndex (n-1) xs)
deleteIndex _ _      = error "illegal usage of deleteIndex"

