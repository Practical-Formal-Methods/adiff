{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VDiff.Prelude.Types
  ( module VDiff.Prelude.Types
  , module Language.C
  , module Language.C.Data.Lens
  , module Language.C.Analysis.AstAnalysis2
  , Type
  , makeFieldsNoPrefix
  , nub
  , MonadRandom(..)
  , voidType
  , VerifierName
  ) where

import           RIO

import           Control.Lens.TH
import           Control.Monad.Random
import           Data.List                           (intersperse, (!!))
import           Data.List.Key                       (nub)
import           Data.Text                           (pack)
import qualified Database.SQLite.Simple              as SQL
import           Language.C                          hiding (LevelError,
                                                      LevelWarn, execParser)
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.SemRep          hiding (Stmt)
import           Language.C.Analysis.TypeUtils       (voidType)
import           Language.C.Data.Lens
import           Safe
import           System.IO                           (FilePath)
import           Text.PrettyPrint                    (render)
import           VDiff.Data
import           VDiff.Instrumentation.Browser.Types


data Strategy = RandomWalkStrategy
              | RandomUniformStrategy
              | RandomUniformBatchStrategy
              | BreadthFirstStrategy
              | DepthFirstStrategy
              | SmartStrategy

data SearchMode = IdentOnly | Subexpressions
  deriving Show

type Microseconds = Int

-- | Every verifier is supposed to run in this environment
data VerifierEnv = VerifierEnv
  { _verifierEnvLogger :: !LogFunc
  , _timeLimit         :: !Microseconds
  , _verifierEnvFlags  :: [Text]
  }


data Verifier = Verifier
  { _name            :: VerifierName
  , _verifierExecute :: FilePath -> RIO VerifierEnv VerifierResult
  , _version         :: IO (Maybe String)
  }

-- | It is important not to execute a Verifier directly through _verifierExecute
-- as the verifiers themselves lack proper exception handling. Use 'execute'!
execute :: Verifier -> FilePath -> RIO VerifierEnv VerifierResult
execute v fp = try (_verifierExecute v fp) >>= \case
  Left (e :: IOException) -> do
    logWarn $ "verifier " <> display (_name v) <> " just caused an IO exception: " <> display (tshow $ displayException e)
    return $ VerifierResult Nothing Nothing Unknown
  Right res -> return res

--------------------------------------------------------------------------------
-- | * RIO
-- | type classes for usage with RIO instances
class HasDatabase a where
  databaseL :: Lens' a SQL.Connection

class (HasLogFunc a, HasDatabase a) => HasMainEnv a

class HasTimeLimit a where
  timeLimitL :: Lens' a Microseconds

class HasExtraFlags a where
  extraFlags :: Lens' a [Text]

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

instance HasLogFunc VerifierEnv where
  logFuncL = lens _verifierEnvLogger (\e l -> e { _verifierEnvLogger = l})

instance HasTimeLimit VerifierEnv where
  timeLimitL = lens _timeLimit (\e l -> e { _timeLimit = l})

instance HasExtraFlags VerifierEnv where
  extraFlags =lens _verifierEnvFlags (\e f -> e { _verifierEnvFlags = f})

-- | creates a verifier environment given a time limit.
mkVerifierEnv :: (HasLogFunc env ) => Microseconds -> [Text] -> RIO env VerifierEnv
mkVerifierEnv timeLimit flags = do
  lg <- view logFuncL
  return $ VerifierEnv lg timeLimit flags

class HasTranslationUnit env where
  translationUnit :: Lens' env (CTranslationUnit SemPhase)

class HasInitialBudget env where
  initialBudget :: Lens' env Int

data StrategyEnv = StrategyEnv
  { _strategyLogFunc         :: !LogFunc
  , _strategyTranslationUnit :: !(CTranslationUnit SemPhase)
  , _strategyDiffParameters  :: !DiffParameters
  , _strategyDatabase        :: !SQL.Connection
  , _strategyInitialBudget   :: !Int
  }

instance HasTranslationUnit StrategyEnv where
  translationUnit = lens _strategyTranslationUnit (\e t -> e {_strategyTranslationUnit = t})

instance HasLogFunc StrategyEnv where
  logFuncL = lens _strategyLogFunc (\e f -> e {_strategyLogFunc = f})

instance HasDiffParameters StrategyEnv where
  diffParameters = lens _strategyDiffParameters (\e p -> e {_strategyDiffParameters = p})

instance HasDatabase StrategyEnv where
  databaseL = lens _strategyDatabase (\e d -> e {_strategyDatabase = d})

instance HasInitialBudget StrategyEnv where
  initialBudget = lens _strategyInitialBudget (\e b -> e {_strategyInitialBudget = b})

instance IsStrategyEnv StrategyEnv

class (HasDiffParameters env, HasTranslationUnit env, HasLogFunc env, HasDatabase env, HasInitialBudget env) => IsStrategyEnv env



data Conclusion
  = StrongAgreement !Verdict       -- ^ all verifiers agree on an outcome
  | WeakAgreement   !Verdict       -- ^ all verifiers that terminate with sat or unsat agree
  | Unsoundness    ![VerifierName] -- ^ verifiers that accept the program although the majority does not
  | Incompleteness ![VerifierName] -- ^ verifiers reject the program although the majority does not
  | Disagreement                  -- ^ none of the other cases
  deriving (Show)

isDisagreement :: Conclusion -> Bool
isDisagreement = \case
  StrongAgreement _ -> False
  WeakAgreement   _ -> False
  Unsoundness     _ -> True
  Incompleteness  _ -> True
  Disagreement      -> True



-- | A "read expression" is a subexpression that contains read variables and
-- that looks (at least syntactically) side-effect free. E.g. a function call is
-- not a read expression, but an array or field access is.
data ExprRead = ExprRead
  { _position   :: AstPosition
  , _expression :: CExpression SemPhase
  } deriving (Show, Eq)


-- | This data type contains all the diff parameters that are passed to the
-- strategy. Note that not all parameters are relevant for all strategies. For
-- example the "batchSize" parameter is only available in random-uniform.
data DiffParameters = DiffParameters
  { _strategy            :: Strategy
  , _budgetSpecification :: Text
  , _timelimit           :: Int
  , _verifiers           :: [Verifier]
  , _verifierFlags       :: Map VerifierName [Text]
  , _searchMode          :: SearchMode
  , _batchSize           :: Int
  , _inputFile           :: FilePath
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

instance MonadRandom (RIO env) where
  getRandomR  = liftIO . getRandomR
  getRandomRs = liftIO . getRandomRs
  getRandom   = liftIO getRandom
  getRandoms  = liftIO getRandoms

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

-- Similar to 'randomlyBranch', but here a branch can fail (by returning
-- @False@) in which another branch is randomly chosen until one branch succeeds
-- or there are no remaining branches
randomlyBranchTrue :: (MonadRandom m) => [m Bool] -> m Bool
randomlyBranchTrue options = isJust <$> randomlyBranchMay options'
  where
    options' = map (boolToMaybe <$>) options
    boolToMaybe False = Nothing
    boolToMaybe True  = Just ()

-- | Partial function
deleteIndex 0 (x:xs) = xs
deleteIndex n (x:xs) = x : deleteIndex (n-1) xs
deleteIndex _ _      = error "illegal usage of deleteIndex"

--------------------------------------------------------------------------------

data NoLogging = NoLogging
instance HasLogFunc NoLogging where
  logFuncL = lens getter setter
    where logFunc = mkLogFunc (\_ _ _ _ -> return ())
          getter = const logFunc
          setter = const $ const NoLogging


isCompound ::Stmt -> Bool
isCompound (CCompound _ _ _ ) = True
isCompound _                  = False


makeFieldsNoPrefix ''Verifier
makeFieldsNoPrefix ''ExprRead

instance Eq Verifier where
  v1 == v2 = (v1 ^. name) == (v2 ^. name)

instance Ord Verifier where
  v1 <= v2 = (v1 ^. name) <= (v2 ^. name)
