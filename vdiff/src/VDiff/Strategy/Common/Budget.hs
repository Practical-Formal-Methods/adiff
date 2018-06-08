{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module VDiff.Strategy.Common.Budget
  ( BudgetT
  , runBudgetT
  , MonadBudget
  , getBudget
  , decrementBudget
  , budgeted
) where

import           VDiff.Prelude

import           Control.Monad.Except
import           Control.Monad.List
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Except (throwE)

newtype BudgetT m a = BudgetT
  { unBudgetT :: StateT Int (ExceptT () m) a
  } deriving (Functor, Applicative, Monad)

instance MonadTrans BudgetT where
  lift x = BudgetT (lift $ lift x)

deriving instance MonadIO (BudgetT (RIO env))
deriving instance MonadRandom (BudgetT (RIO env))
deriving instance MonadReader env (BudgetT (RIO env))

-- TODO: Think about the right return type here
runBudgetT :: (Monad m) => Int -> BudgetT m a -> m (Maybe a, Int)
runBudgetT bdg act = do
  y <- runExceptT $ runStateT (unBudgetT act) bdg
  case y of
    Left _         -> return (Nothing, 0)
    Right (x,bdg') -> return (Just x, bdg')


class (Monad m) => MonadBudget m where
  getBudget       :: m Int
  decrementBudget :: m ()
  budgetError     :: m a

instance (Monad m) => MonadBudget (BudgetT m) where
  getBudget       = BudgetT get
  decrementBudget = BudgetT (modify' (\x -> x - 1))
  budgetError     = BudgetT $ lift $ throwE ()

-- | Performs action only if the budget is at least 1.
budgeted :: (MonadBudget m) => (m a) -> m a
budgeted act = do
  b <- getBudget
  if b > 0
    then decrementBudget >> act
    else budgetError
