{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Implements the core instrumentation functions.
module VDiff.Instrumentation
 (
   -- * Handling C files
  openCFile
 , prettyp
 ,  maskAsserts
   -- * Zipping
   -- $zipping
 , Stmt
 , Direction(..)
 , MonadBrowser
 , BrowserT(..)
 , runBrowserT
 , findReads
 , insertBefore
 , buildTranslationUnit
 , tryout
 , go
 , goto
 , currentStmt
 , currentPosition
 , go_
 , AstPosition(..)
 -- * Internals
 , insertBeforeNthStatement
 , markAllReads
 ) where

import qualified Prelude                          as P
import           RIO                              hiding ((^.))

import           Control.Lens.Cons
import           Control.Lens.Getter              (use)
import           Control.Lens.Operators
import           Control.Lens.Setter              (mapped)
import           Control.Lens.TH
import           Control.Monad.State.Strict
import           Data.Functor.Identity
import           Data.Generics.Uniplate.Data      ()
import           Data.Generics.Uniplate.Zipper    (fromZipper)
import qualified Data.Generics.Uniplate.Zipper    as Z
import           Data.Text                        (pack)
import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.SemRep       hiding (Stmt)
import           Language.C.Analysis.TravMonad
import           Language.C.Analysis.TypeUtils
import           Language.C.Data.Lens
import           Language.C.System.GCC
import           Text.PrettyPrint                 (render)

import           VDiff.Types


instance Display (CStatement a) where
  display = display . pack . prettyp

prettyp :: Pretty a => a -> String
prettyp = render . Language.C.pretty

-- | short-hand for open, parse and type annotate, will log parse and type checking errors and warnings.
openCFile :: HasLogFunc env => FilePath -> RIO env (Maybe (CTranslationUnit SemPhase))
openCFile fn = do
  x <- liftIO $ parseCFile (newGCC "gcc") Nothing [] fn
  case x of
    Left parseError -> do
      logError $ "parse error: " <> displayShow parseError
      return Nothing
    Right tu -> case runTrav_ (analyseAST tu) of
        Left typeError -> do
          logError $ "type error: " <> displayShow typeError
          return Nothing
        Right (tu', warnings) -> do
          unless (null warnings) $ logWarn $ "warnings: " <> displayShow warnings
          return (Just tu')

--------------------------------------------------------------------------------
type Stmt = CStatement SemPhase
type StmtZipper= Z.Zipper Stmt Stmt


-- | find reads in a statement
readsStatement :: Stmt -> [(Ident,Type)]
readsStatement s = case s of
  (CExpr (Just e) _)  -> readsExpression e
  (CExpr Nothing _)   -> []
  (CIf e _ _ _)       -> readsExpression e
  (CWhile e _ _ _)    -> readsExpression e
  (CLabel _ stmt _ _) -> readsStatement stmt
  (CSwitch e _ _)     -> readsExpression e
  (CFor (Left me1) me2 me3 _ _) -> concat $ catMaybes $ (fmap.fmap $ readsExpression) [me1, me2, me3]
  (CFor (Right decl) me2 me3 _ _) -> let reads2 = concat $ catMaybes $ (fmap.fmap $ readsExpression) [me2, me3]
                                         (reads1, exclude) = readsDeclaration decl
                                     in filter (\x -> fst x `notElem` exclude)  (reads1 ++ reads2)
  _                   -> []

  where
    readsExpression (CVar n (_,ty))    = [(n, ty)]
    readsExpression (CBinary _ l r _)  = readsExpression l <> readsExpression r
    readsExpression (CUnary _ e _)     = readsExpression e
    readsExpression (CAssign _ _ e2 _) = readsExpression e2
    readsExpression (CCall _ es _)     = concatMap readsExpression es
    readsExpression _                  = []

    readsInitializer :: CInitializer SemPhase -> [(Ident,Type)]
    readsInitializer (CInitExpr e _) = readsExpression e
    readsInitializer (CInitList lst _) = concat $ map (readsInitializer.snd) lst

    readsDeclaration :: CDeclaration SemPhase -> ([(Ident, Type)], [Ident])
    readsDeclaration (CDecl _ declrs _) = (reads, declared)
      where
        reads        = concatMap readsInitializer initializers :: [(Ident,Type)]
        initializers = catMaybes $ map (\(_,x,_) -> x) declrs
        declarators  = catMaybes $ map (\(x,_,_) -> x) declrs :: [CDeclarator SemPhase]
        declared     = catMaybes $ map identifier declarators
          where identifier (CDeclr mi _ _ _ _) = mi
    readsDeclaration _ = ([],[])

--------------------------------------------------------------------------------
-- $zipping
-- When you want to modify an AST, you probably want to use a zipper. You can use it with any state monad, just make sure it is an instance of 'ZipperState', meaning that it has to store a 'StmtZipper' and a 'siblingIndex', where the latter is counting the skipped statements in the current compound statement.
-- Inside your state monad you can then use functions like 'go','go_'...
--------------------------------------------------------------------------------
data Direction = Up | Down | Next | Prev
  deriving (Eq, Enum, Bounded, Show)

-- this is what every strategy needs to move around in the AST
data BrowserState = BrowserState
  { _stmtZipper   :: !StmtZipper
  , _stmtPosition :: ![Int]
  }

makeFieldsNoPrefix ''BrowserState


newtype BrowserT m a = BrowserT
  { unBrowserT :: StateT BrowserState m a
  } deriving (Functor, Applicative, Monad, MonadState BrowserState, MonadTrans)


runBrowserT :: (Monad m) => BrowserT m a -> Stmt -> m (a, Stmt)
runBrowserT a s = do
  (x, bs) <- runStateT (unBrowserT a) (BrowserState (Z.zipper s) [0])
  return (x, fromZipper $ bs ^. stmtZipper)

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


data ZipperException = ZipperException
  deriving Show

instance Exception ZipperException


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

newtype AstPosition = AstPosition
  { indices :: [Int]
  } deriving (Eq, Ord, Show)

instance Display AstPosition where
 display (AstPosition xs) = foldl' f "" xs
  where f b x = display b <> "/" <> display x

-- Important note: a position contains the indices ordered from top to bottom
-- whereas the stmtPosition in the BrowserState is from bottom to top.

currentPosition :: MonadBrowser m => m (AstPosition)
currentPosition = do
  st <- getBrowserState
  return $ AstPosition (reverse $ st ^. stmtPosition)

-- TODO: Improve implementation
goto :: (MonadBrowser m) => AstPosition -> m ()
goto (AstPosition xs) = do
  --- move to the top
  toTop
  goto' xs
  where
    toTop = go Up >>= \u -> if u then toTop else return ()
    goto' []     = error "a position should never be empty"
    goto' [y]    = gotoSibling y
    goto' (y:ys) = gotoSibling y >> go Down >> goto' ys



-- | Move to the nth sibling. Be careful!
gotoSibling :: (MonadBrowser m) => Int -> m ()
gotoSibling n = do
  (p:_) <- _stmtPosition <$> getBrowserState
  let diff = n - p
  if
    | diff > 0 -> replicateM_ diff (go_ Next)
    | diff < 0 -> replicateM_ (- diff) (go_ Prev)
    | otherwise -> return ()


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

findReads :: (MonadBrowser m) => m [(Ident, Type)]
findReads = do
  s <- currentStmt
  return $ readsStatement s

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

buildTranslationUnit :: (MonadBrowser m, MonadReader env m, HasTranslationUnit env) => m (CTranslationUnit SemPhase)
buildTranslationUnit = do
  tu <- view translationUnit
  st <- getBrowserState
  let stmt = fromZipper (st ^. stmtZipper)
  let modif = (ix "main" . functionDefinition . body) .~ stmt
  return $ modif tu




-- | NOTE: Like go, but throws an error when doing it can't go into the given direction
go_ :: (MonadBrowser m) => Direction -> m ()
go_ d = do
  m <- go d
  unless m $ error ("cannot go " ++ show d)





markAllReads :: CTranslationUnit SemPhase-> CTranslationUnit SemPhase
markAllReads = externalDeclarations . mapped . functionDefinition . body  %~ markAllReadsStmt

markAllReadsStmt :: Stmt -> Stmt
markAllReadsStmt s = snd <$> runIdentity $ runBrowserT (explore [0]) s

-- first parameter is the number of explored siblings per level (deepest first)
explore :: [Int] -> BrowserT Identity ()
explore st = do
  v <- findReads
  unless (null v) $ insertBefore $ mkReadMarker v
  d <- go Down
  if d
    then do
      (n:_) <- use stmtPosition
      explore (n : st)
    else do
       x <- go Next
       if x
         then explore st
         else ascend st
  where
    ascend [] = return ()
    ascend (n:ns) =
      whenM (go Up) $ do
        replicateM_ n (go_ Next)
        new <- go Next
        if new
          then explore ns
          else ascend ns


--------------------------------------------------------------------------------
-- | * Masking
--------------------------------------------------------------------------------
-- NOTE: This is soooo mechanical.  recursion-schemes or lenses to the rescue?
applyOnExpr :: (CExpression a -> CExpression a) -> CTranslationUnit a -> CTranslationUnit a
applyOnExpr f (CTranslUnit eds as) = CTranslUnit (map externalDeclaration eds) as
  where
    externalDeclaration (CFDefExt fundef) = CFDefExt (functionDefinition' fundef)
    externalDeclaration d = d

    functionDefinition' (CFunDef specs declr decls stmt a) = CFunDef specs declr decls (statement stmt) a

    statement (CLabel i stmt attrs a)         = CLabel i (statement stmt) attrs a
    statement (CCase expr stmt a)             = CCase (f expr) (statement stmt) a
    statement (CCases expr1 expr2 stmt a)     = CCases (f expr1) (f expr2) (statement stmt) a
    statement (CDefault stmt a)               = CDefault (statement stmt) a
    statement (CExpr Nothing a)               = CExpr Nothing a
    statement (CExpr (Just e) a)              = CExpr (Just (f e)) a
    statement (CCompound is blkItems a)       = CCompound is (map blkItem blkItems) a
    statement (CIf e s Nothing a)             = CIf (f e) (statement s) Nothing a
    statement (CIf e s (Just s2) a)           = CIf (f e) (statement s) (Just (statement s2)) a
    statement (CSwitch e s a)                 = CSwitch (f e) (statement s) a
    statement (CWhile e s b a)                = CWhile (f e) (statement s) b a
    statement (CFor (Left me) me2 me3 stmt a) = CFor (Left $ f <$> me) (f <$> me2) (f <$> me3) (statement stmt) a
    statement (CGotoPtr e a)                  = CGotoPtr (f e) a
    statement (CReturn me a)                  = CReturn (f <$> me) a
    statement s                               = s

    blkItem (CBlockStmt stmt)      = CBlockStmt (statement stmt)
    blkItem (CBlockDecl decl)      = CBlockDecl (declaration decl)
    blkItem (CNestedFunDef fundef) = CNestedFunDef (functionDefinition' fundef)

    declaration (CDecl specs declrs a) = CDecl specs (map (\(md,mi,me) -> (md, initializer <$> mi, f <$> me) ) declrs) a
    declaration d                      = d

    initializer (CInitExpr e a) = CInitExpr (f e) a
    initializer (CInitList initList a) = CInitList (map (\(p,i) -> (p, initializer i)) initList) a




maskAsserts :: CTranslationUnit SemPhase -> CTranslationUnit SemPhase
maskAsserts = insertDummy . applyOnExpr rename
  where
    rename v@(CVar i a)
      | identToString i == "__VERIFIER_assert" = CVar (internalIdent "__DUMMY_VERIFIER_assert") a
      | otherwise = v
    rename (CCall e1 es a)      = CCall (rename e1) (map rename es) a
    rename (CComma es a)        = CComma (map rename es) a
    rename (CAssign op e1 e2 a) = CAssign op (rename e1) (rename e2) a
    rename (CCond e me e2 a)    = CCond  (rename e) (rename <$> me) (rename e2) a
    rename (CBinary op e1 e2 a) = CBinary op (rename e1) (rename e2) a
    rename (CCast d e a)        = CCast d (rename e) a
    rename (CUnary op e a)      = CUnary op (rename e) a
    rename (CSizeofExpr e a)    = CSizeofExpr (rename e) a
    rename (CIndex e1 e2 a)     = CIndex (rename e1) (rename e2) a
    rename (CMember e1 n b a)   = CMember (rename e1) n b a
    rename e = e

    insertDummy (CTranslUnit exts a)  = CTranslUnit exts' a
      where exts' = CFDefExt dummyAssert : exts

--------------------------------------------------------------------------------
-- | some simple AST constructors
--------------------------------------------------------------------------------

mkReadMarker ::  [(Ident,Type)] -> Stmt
mkReadMarker vars =
  let fun = CVar (builtinIdent "__VERIFIER_read") (undefNode,voidType)
      expressions = map (\(i,ty) -> CVar i (undefNode, ty)) vars
  in CExpr (Just $ CCall fun expressions  (undefNode,voidType)) (undefNode,voidType)

-- | This is a function definition defining the function __dummy__verifier_assert that does nothing
-- | (Existing asserts will be disabled by renaming things to this function's name)
-- | NOTE: (to myself) a quasi-quoter would be really nice, or at least some exported sub-parsers.
dummyAssert :: CFunctionDef SemPhase
dummyAssert = CFunDef specs decl [] body' undefNode
  where specs = [CTypeSpec (CVoidType undefNode)]
        decl = CDeclr  (Just $ internalIdent "__DUMMY_VERIFIER_assert" ) derived Nothing [] undefNode
        derived = [CFunDeclr (Right ([param], False)) [] undefNode]
        param = CDecl [CTypeSpec $ CIntType undefNode] [(Just paramDecl, Nothing, Nothing)]  undefNode
        paramDecl = CDeclr (Just $ internalIdent "condition") [] Nothing [] undefNode :: CDeclarator SemPhase
        body' = CCompound [] [] (undefNode, voidType)

--------------------------------------------------------------------------------
-- simple utilities
--------------------------------------------------------------------------------
-- | partial!
insertBeforeNthStatement :: CStatement a -> Int -> [CCompoundBlockItem a] -> [CCompoundBlockItem a]
insertBeforeNthStatement s 0 items@(CBlockStmt _ : _) = CBlockStmt s : items
insertBeforeNthStatement s n (x@(CBlockStmt _):xs)    = x : insertBeforeNthStatement s (n-1) xs
insertBeforeNthStatement s n (x:xs)                   = x : insertBeforeNthStatement s n xs
insertBeforeNthStatement _  _ _                       = error "illegal insertAt"
