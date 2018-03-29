{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Instrumentation
 ( maskAsserts
 , ZipperState(..)
 , StmtZipper
 , Stmt
 , Direction(..)
 , go
 , go_
 , findReads
 , tryout
 , insertBefore
 , mkZipper
 , fromZipper
 , currentStmt
 , printCurrentStmt
 , insertBeforeNthStatement
 , markAllReads
 ) where

import qualified Prelude                          as P
import           RIO

import           Debug.Trace
import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.SemRep       hiding (Stmt)
import           Language.C.Analysis.TypeUtils
import           Language.C.Data.Lens
import           Text.PrettyPrint                 (render)

import           Data.Generics.Uniplate.Data      ()
import           Data.Generics.Uniplate.Zipper    (fromZipper)
import qualified Data.Generics.Uniplate.Zipper    as Z

import           Control.Lens.Getter              (use)
import           Control.Lens.Operators           ((%=), (%~), (+=), (-=), (.=))
import           Control.Lens.Setter              (mapped)
import           Control.Monad.State

--------------------------------------------------------------------------------
type Stmt = CStatement SemPhase
type StmtZipper= Z.Zipper Stmt Stmt

-- | find reads in a statement
readsStatement :: Stmt -> [(Ident,Type)]
readsStatement s = case s of
  (CExpr (Just e) _)  -> readsExpression e
  (CExpr Nothing _)   -> []
  (CIf e _ _ _)       -> readsExpression e -- [internalIdent "x"]
  (CWhile e _ _ _)    -> readsExpression e
  (CLabel _ stmt _ _) -> readsStatement stmt
  _                   -> []

  where
    readsExpression (CVar n (_,ty))    = [(n, ty)]
    readsExpression (CBinary _ l r _)  = readsExpression l <> readsExpression r
    readsExpression (CUnary _ e _)     = readsExpression e
    readsExpression (CAssign _ _ e2 _) = readsExpression e2
    readsExpression _                  = []

--------------------------------------------------------------------------------
-- | * Zipping
--------------------------------------------------------------------------------
mkZipper :: Stmt -> StmtZipper
mkZipper = Z.zipper

-- this is what every strategy needs to move around in the AST
class ZipperState state where
  stmtZipper    :: Lens' state StmtZipper
  siblingIndex :: Lens' state Int
  {-# MINIMAL stmtZipper, siblingIndex #-}


data Direction = Up | Down | Next | Prev
  deriving (Eq, Show)

data ZipperException = ZipperException
  deriving Show

instance Exception ZipperException

-- | tries to move the zipper into the given direction. returns true if successful.
go :: (Monad m, ZipperState st, MonadState st m) => Direction -> m Bool
go d = trace (show d) $ do
  p <- use stmtZipper
  let f = case d of
        Prev -> Z.left
        Next -> Z.right
        Up   -> Z.up
        Down -> Z.down
  case f p of
    Nothing -> return False
    Just z -> do
      case d of
        Up   -> siblingIndex .= 0
        Down -> siblingIndex .= 0
        Prev -> siblingIndex -= 1
        Next -> siblingIndex += 1
      stmtZipper .= z
      return True

-- | NOTE: Like go, but throws an error when doing it can't go into the given direction
go_ :: (Monad m, ZipperState st, MonadState st m) => Direction -> m ()
go_ d = do
  m <- go d
  unless m $ do
    z <- use stmtZipper
    trace ("currently at: " ++ showp (Z.hole z)) $ error "should not happen"

insertBefore :: (MonadState st m, ZipperState st) => Stmt -> m ()
insertBefore ins = do
  trace "--- start insert" $ return ()
  si <- use siblingIndex
  -- move up
  go_ Up
  p <- use stmtZipper
  -- check that we are in a compound statement and replace the compound statement
  case Z.hole p of
    (CCompound l items ann) -> do
          let items' = insertBeforeNthStatement ins si items
              s' =  CCompound l items' ann
          stmtZipper %= Z.replaceHole s'
    _               -> error "insertBefore was called at a location outside of a compound statement"
  -- move back to the original position
  go_ Down
  replicateM_ (si+1) (go Next)
  trace "----- end insert " $ return ()

currentStmt :: (MonadState st m, ZipperState st) => m Stmt
currentStmt = do
  p <- use stmtZipper
  return $ Z.hole p

printCurrentStmt :: (MonadState st m, ZipperState st, MonadIO m) => m ()
printCurrentStmt = do
  s <- currentStmt
  liftIO $ P.putStrLn $ render $ pretty s

showp :: (Pretty p) => p -> String
showp = render . pretty


-- this will reset the zipper back to how it was
-- somehow store some information on stack, so we know that after some time we
-- might want to come back out
tryout :: (ZipperState st, MonadState st m) => m a -> m a
tryout act = do
  z <- use stmtZipper
  x <- act
  stmtZipper .= z
  return x

data TraverseState = TraverseState
  { _traverseZipper       ::  StmtZipper
  , _traverseSiblingIndex :: Int
  }
instance ZipperState TraverseState where
  stmtZipper = lens _traverseZipper $ \s z -> s {_traverseZipper = z}
  siblingIndex = lens _traverseSiblingIndex $ \s i -> s {_traverseSiblingIndex = i}

markAllReads :: CTranslationUnit SemPhase-> CTranslationUnit SemPhase
markAllReads = externalDeclarations . mapped . functionDefinition . body  %~ markAllReadsStmt

markAllReadsStmt :: Stmt -> Stmt
markAllReadsStmt s =
  let (_, st) = runState (horizontal [0]) (TraverseState (mkZipper s) 0 )
  in fromZipper $ st ^. stmtZipper

horizontal :: [Int] -> State TraverseState ()
horizontal []     = return ()
horizontal (n:ns) =
  -- go to given position
  whenM (and <$> replicateM n (go Next)) $
    -- go vertical
    vertical (n + 1 : ns)

vertical :: [Int] -> State TraverseState ()
vertical h = do
  v <- findReads
  unless (null v) $ insertBefore $ mkReadMarker v
  -- depth-first
  b <- go Down
  if b then horizontal (0 : h)
  else do
    b' <- go Next
    if b' then vertical h
    else go Up >> horizontal h

-- pop :: (Monad m) => StateT TraverseState m (Maybe Int)
-- pop = do
--   s <- use traverseCompleted
--   traverseCompleted %= tailSafe -- remove first element
--   return $ headMay s

--------------------------------------------------------------------------------
-- | * Finding reads/writes
--------------------------------------------------------------------------------

findReads :: (Monad m, ZipperState st) => StateT st m [(Ident, Type)]
findReads = do
  p <- use stmtZipper
  return $ readsStatement (Z.hole p)


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
-- | simple utilities
--------------------------------------------------------------------------------
-- | partial!
insertBeforeNthStatement :: CStatement a -> Int -> [CCompoundBlockItem a] -> [CCompoundBlockItem a]
insertBeforeNthStatement s 0 items@(CBlockStmt _ : _) = CBlockStmt s : items
insertBeforeNthStatement s n (x@(CBlockStmt _):xs)    = x : insertBeforeNthStatement s (n-1) xs
insertBeforeNthStatement s n (x:xs)                   = x : insertBeforeNthStatement s n xs
insertBeforeNthStatement _  _ _                       = error "illegal insertAt"

whenM :: Monad m => m Bool -> m () -> m ()
whenM p t   = p >>= flip when t
