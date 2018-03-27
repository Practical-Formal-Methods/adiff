{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Instrumentation
 ( maskAsserts
 , ZipperState(..)
 , StmtZipper
 , Stmt
 , Direction(..)
 , go
 , go_
 , hole
 , findReads
 , tryout
 , insertBefore
 ) where

import           RIO

import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.SemRep       hiding (Stmt)
import           Language.C.Analysis.TypeUtils
import           Text.PrettyPrint                 (render)

import           Debug.Trace

import           Data.Generics.Uniplate.Data ()
import           Data.Generics.Uniplate.Zipper

import           Control.Lens.Getter              (use)
import           Control.Lens.Operators           ((%=), (+=), (-=), (.=))
import           Control.Monad.State

--------------------------------------------------------------------------------
type Stmt = CStatement SemPhase
type StmtZipper= Zipper Stmt Stmt

-- | find reads in a statement
readsStatement :: Stmt -> [(Ident,Type)]
readsStatement s = trace ("reads in " ++ show (render (pretty s))) (case s of
  (CExpr (Just e) _)  -> readsExpression e
  (CExpr Nothing _)   -> []
  (CIf e _ _ _)       -> readsExpression e -- [internalIdent "x"]
  (CWhile e _ _ _)    -> readsExpression e
  (CLabel _ stmt _ _) -> readsStatement stmt
  _                   -> []
  )
  where
    readsExpression (CVar n (_,ty))   = [(n, ty)]
    readsExpression (CBinary _ l r _) = readsExpression l <> readsExpression r
    readsExpression (CUnary _ e _)    = readsExpression e
    readsExpression _                 = []

--------------------------------------------------------------------------------
-- | * Zipping
--------------------------------------------------------------------------------

-- this is what every strategy needs to move around in the AST
class ZipperState state where
  stmtZipper    :: Lens' state StmtZipper
  siblingIndex :: Lens' state Int


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
        Prev -> left
        Next -> right
        Up   -> up
        Down -> down
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
  unless m $ error "should not happen"

insertBefore :: (MonadThrow m, MonadState st m, ZipperState st) => Stmt -> m ()
insertBefore ins = do
  si <- use siblingIndex
  trace ("inserting something at " ++ show si) (return ())
  -- move up
  go_ Up
  p <- use stmtZipper
  -- check that we are in a compound statement and replace the compound statement
  case hole p of
    (CCompound l items ann) -> do
          let items' = insertAt si (CBlockStmt ins) items
              s' =  CCompound l items' ann
          stmtZipper %= replaceHole s'
    _               -> throwM ZipperException
  -- move back to the original position
  go_ Down
  replicateM_ si (go Next)

-- this will reset the zipper back to how it was
-- somehow store some information on stack, so we know that after some time we
-- might want to come back out
tryout :: (ZipperState st, MonadState st m) => m a -> m a
tryout act = do
  z <- use stmtZipper
  x <- act
  stmtZipper .= z
  return x

--------------------------------------------------------------------------------
-- | * Finding reads/writes
--------------------------------------------------------------------------------

findReads :: (Monad m, ZipperState st) => StateT st m [(Ident, Type)]
findReads = do
  p <- use stmtZipper
  x <- use siblingIndex
  let vars = readsStatement (hole p)
  trace ("found vars at position " ++ show x ++ show vars) $ return vars


--------------------------------------------------------------------------------
-- | * Masking
--------------------------------------------------------------------------------
-- NOTE: This is soooo mechanical.  recursion-schemes or lenses to the rescue?
applyOnExpr :: (CExpression a -> CExpression a) -> CTranslationUnit a -> CTranslationUnit a
applyOnExpr f (CTranslUnit eds as) = CTranslUnit (map externalDeclaration eds) as
  where
    externalDeclaration (CFDefExt fundef) = CFDefExt (functionDefinition fundef)
    externalDeclaration d = d

    functionDefinition (CFunDef specs declr decls stmt a) = CFunDef specs declr decls (statement stmt) a

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
    blkItem (CNestedFunDef fundef) = CNestedFunDef (functionDefinition fundef)

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
dummyAssert = CFunDef specs decl [] body undefNode
  where specs = [CTypeSpec (CVoidType undefNode)]
        decl = CDeclr  (Just $ internalIdent "__DUMMY_VERIFIER_assert" ) derived Nothing [] undefNode
        derived = [CFunDeclr (Right ([param], False)) [] undefNode]
        param = CDecl [CTypeSpec $ CIntType undefNode] [(Just paramDecl, Nothing, Nothing)]  undefNode
        paramDecl = CDeclr (Just $ internalIdent "condition") [] Nothing [] undefNode :: CDeclarator SemPhase
        body = CCompound [] [] (undefNode, voidType)

--------------------------------------------------------------------------------
-- | simple utilities
--------------------------------------------------------------------------------
-- | partial!
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 y xs     = y:xs
insertAt n y (_:xs) = insertAt (n-1) y xs
insertAt _ _ _      = error "illegal insertAt"

-- whenM, unlessM :: Monad m => m Bool -> m () -> m ()
-- whenM p t   = p >>= flip when t
-- unlessM p e = p >>= flip unless e
