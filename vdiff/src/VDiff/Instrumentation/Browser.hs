{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : VDiff.Instrumentation.Browser
Description : Provides a simple way to move in the AST and modify it.

BrowserT adds a zipper (think of iterator that can move into multiple directions) to any monad.
One can move in the AST by using 'go', 'gotoPosition', and 'gotoFunction'. Modifications can be made with 'insertBefore'.
At any point, 'buildTranslationUnit' yields the complete translation unit. One
can use @tryout@ to apply modifications locally.

-}
module VDiff.Instrumentation.Browser
  (
  -- * Basics
   MonadBrowser(..)
  , BrowserT(..)
  , runBrowserT
  , Browser
  , runBrowser
  , AstPosition(..)
  , astDepth
  , Direction(..)
  , tryout
  -- * Operations at the current position
  , currentFunction
  , currentStmt
  , currentPosition
  , insertBefore
  , insertBeforeNthStatement
  , buildTranslationUnit
  -- * Movements
  , gotoFunction
  , gotoPosition
  , go
  , go_
  , stmtPosition -- TODO: maybe not export this
  ) where


import qualified Prelude                       as P
import           RIO                           hiding ((^.))
import           VDiff.Types

import           Control.Lens
import           Control.Monad.State.Strict
import           Control.Monad.Writer          hiding ((<>))
import           Data.Generics.Uniplate.Data   ()
import qualified Data.Generics.Uniplate.Zipper as Z

type StmtZipper= Z.Zipper Stmt Stmt

data Direction = Up | Down | Next | Prev
  deriving (Eq, Enum, Bounded, Show)

-- this is what every strategy needs to move around in the AST
-- TODO: Move the "originalTranslationUnit" to a reader instance
data BrowserState = BrowserState
  { _stmtZipper      :: !StmtZipper
  , _stmtPosition    :: ![Int]
  , _currentFunction :: String -- ^ name of the current function
  , _currentTU       :: CTranslationUnit SemPhase
  }
makeFieldsNoPrefix ''BrowserState


newtype BrowserT m a = BrowserT
  { unBrowserT :: (StateT BrowserState m) a
  } deriving (Functor, Applicative, Monad, MonadState BrowserState, MonadTrans)

type Browser a = BrowserT Identity a

runBrowser :: Browser a -> CTranslationUnit SemPhase -> (a, CTranslationUnit SemPhase)
runBrowser b = runIdentity . runBrowserT b

-- | executes an @BrowserT@ action on the given translation unit. The initial position is always the body of the main function.
runBrowserT :: (Monad m) => BrowserT m a -> CTranslationUnit SemPhase -> m (a, CTranslationUnit SemPhase)
runBrowserT a tu = do
  let (Just mainFunBody) = tu ^? (ix "main" . functionDefinition . body)
      zp = Z.zipper mainFunBody
      initialState = BrowserState zp [0] "main" tu
  (x, bs :: BrowserState) <- runStateT (unBrowserT a) initialState
  let
      stmt' = Z.fromZipper ((bs ^. stmtZipper) :: StmtZipper ) :: Stmt
      tu' = tu & (ix (bs ^. currentFunction) . functionDefinition . body) .~ stmt'
  return (x, tu')

class (Monad m) => MonadBrowser m where
  putBrowserState :: BrowserState -> m ()
  getBrowserState :: m BrowserState
  modifyBrowserState :: (BrowserState -> BrowserState) -> m ()
  modifyBrowserState f = getBrowserState >>= putBrowserState . f

instance (Monad m) => MonadBrowser (BrowserT m) where
  putBrowserState = put
  getBrowserState = get

--------------------------------------------------------------------------------
deriving instance MonadIO (BrowserT IO)
deriving instance MonadReader env (BrowserT (RIO env))
deriving instance MonadIO (BrowserT (RIO env))

instance (MonadBrowser m) => MonadBrowser (StateT s m) where
  putBrowserState st = lift $ putBrowserState st
  getBrowserState    = lift getBrowserState


instance (MonadBrowser m, Monoid w) => MonadBrowser (WriterT w m) where
  putBrowserState st = lift $ putBrowserState st
  getBrowserState = lift getBrowserState

-- | tries to move the zipper into the given direction. returns true if successful.
go :: (MonadBrowser m) => Direction -> m Bool
go d = do
  st <- getBrowserState
  let f = case d of
        Prev -> Z.left
        Next -> Z.right
        Up   -> Z.up
        Down -> Z.down
  case f (st ^. stmtZipper) of
    Nothing -> return False
    Just z -> do
      let st' = case d of
            Up   -> (stmtPosition %~ P.tail) st -- pop
            Down -> (stmtPosition %~ (0:)) st  -- push 0
            Prev -> (stmtPosition . _head -~ 1) st
            Next -> (stmtPosition . _head +~ 1) st
      putBrowserState $ (stmtZipper .~ z) st'
      return True

-- |  Works similarly to 'go', but crashes when it cannot go into the given direction.
-- NOTE: Use only when you are sure that the movement will succeed.
go_ :: (MonadBrowser m) => Direction -> m ()
go_ d = do
  m <- go d
  unless m $ error ("cannot go " ++ show d)

-- | Describes the position of a statement in the translation unit by encoding a "path" through the abstract syntax tree.
data AstPosition = AstPosition
  { functionName :: String
  , indices      :: [Int]
  } deriving (Eq, Ord, Show)

astDepth :: AstPosition -> Int
astDepth (AstPosition _ is) = length is


instance Display AstPosition where
 display (AstPosition fn xs) = display (tshow fn) <> "/" <> foldl' f "" xs
  where f b x = display b <> "/" <> display x

-- Important note: a position contains the indices ordered from top to bottom
-- whereas the stmtPosition in the BrowserState is from bottom to top.

currentPosition :: MonadBrowser m => m AstPosition
currentPosition = do
  st <- getBrowserState
  let fn = st ^. currentFunction
      pos = st ^. stmtPosition
  return $ AstPosition fn (reverse pos)

-- | moves to the given position. TODO: Improve implementation
gotoPosition :: (MonadBrowser m) => AstPosition -> m ()
gotoPosition (AstPosition fn xs) = do
  -- goto function
  gotoFunction fn
  -- move down with the given indices
  goto' xs
  where
    goto' []     = error "a position should never be empty"
    goto' [y]    = gotoSibling y
    goto' (y:ys) = gotoSibling y >> go Down >> goto' ys


gotoFunction :: (MonadBrowser m) => String -> m ()
gotoFunction fn = do
  st <- getBrowserState
  let cTU                 = st ^. currentTU
      currentZip          = st ^. stmtZipper
      currentFunctionName = st ^. currentFunction :: String
      tu'                 = cTU & (ix currentFunctionName . functionDefinition . body) .~ Z.fromZipper currentZip
      (Just fbody)        = tu' ^? (ix fn . functionDefinition . body)
      newZipper           = Z.zipper fbody
  let st' =  st & (currentTU .~ tu')
                & (currentFunction .~ fn)
                & (stmtZipper .~ newZipper)
                & (stmtPosition .~ [0])
  putBrowserState st'


-- | Move to the nth sibling. Be careful!
gotoSibling :: (MonadBrowser m) => Int -> m ()
gotoSibling n = do
  (p:_) <- _stmtPosition <$> getBrowserState
  let diff = n - p
  if
    | diff > 0 -> replicateM_ diff (go_ Next)
    | diff < 0 -> replicateM_ (- diff) (go_ Prev)
    | otherwise -> return ()

currentStmt :: (MonadBrowser m) => m Stmt
currentStmt = do
  st <- getBrowserState
  return $ Z.hole (st ^. stmtZipper)


-- | This executes a monadic actions but resets the zipper to the value it previously had. This is convenient in combination of zipper modifying functions like 'insertBefore'.

tryout :: MonadBrowser m => m a -> m a
tryout act = do
  st <- getBrowserState
  x <- act
  putBrowserState st
  return x

insertBefore :: (MonadBrowser m) => Stmt -> m ()
insertBefore ins = do
  st <- getBrowserState
  let (n:_) = st ^. stmtPosition
  -- move up
  go_ Up
  xx <- currentStmt
  -- check that we are in a compound statement and replace the compound statement
  case xx of
    (CCompound l items ann) -> do
          let items' = insertBeforeNthStatement ins n items
              s' =  CCompound l items' ann
          modifyBrowserState $ stmtZipper %~ Z.replaceHole s'
    _               -> error "insertBefore was called at a location outside of a compound statement"
  -- move back to the original position
  go_ Down
  replicateM_ (n+1) (go Next)


buildTranslationUnit :: (MonadBrowser m) => m (CTranslationUnit SemPhase)
buildTranslationUnit = do
  st <- getBrowserState
  let stmt = Z.fromZipper (st ^. stmtZipper)
      tu = st ^. currentTU
      fn = st ^. currentFunction
  return $ tu & (ix fn . functionDefinition . body) .~ stmt




--------------------------------------------------------------------------------
-- simple utilities
--------------------------------------------------------------------------------
-- | partial!
insertBeforeNthStatement :: CStatement a -> Int -> [CCompoundBlockItem a] -> [CCompoundBlockItem a]
insertBeforeNthStatement s 0 items@(CBlockStmt _ : _) = CBlockStmt s : items
insertBeforeNthStatement s n (x@(CBlockStmt _):xs)    = x : insertBeforeNthStatement s (n-1) xs
insertBeforeNthStatement s n (x:xs)                   = x : insertBeforeNthStatement s n xs
insertBeforeNthStatement _  _ _                       = error "illegal insertAt"
