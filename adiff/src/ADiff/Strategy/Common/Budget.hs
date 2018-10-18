-- MIT License
--
-- Copyright (c) 2018 Christian Klinger
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module ADiff.Strategy.Common.Budget
  ( BudgetT
  , runBudgetT
  , MonadBudget
  , getBudget
  , decrementBudget
  , budgeted
) where

import           ADiff.Prelude

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
budgeted :: (MonadBudget m) => m a -> m a
budgeted act = do
  b <- getBudget
  if b > 0
    then decrementBudget >> act
    else budgetError
