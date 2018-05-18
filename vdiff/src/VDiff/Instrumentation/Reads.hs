{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module VDiff.Instrumentation.Reads where

import           RIO

import           Control.Monad.Writer              hiding ((<>))
import qualified Data.DList                        as DL
import           Data.Functor.Identity
import           Data.Generics.Uniplate.Operations
import           Data.List                         (intersect)
import           Language.C.Analysis.TypeUtils     (isIntegralType)
import           VDiff.Instrumentation.Browser
import qualified VDiff.Instrumentation.Fragments   as Fragments
import           VDiff.Types

data VarRead = VarRead
  { _position   :: AstPosition
  , _identifier :: Ident
  , _varType    :: Type
  } deriving (Show, Eq)
makeFieldsNoPrefix ''VarRead


-- | A "read expression" is a subexpression that contains read variables and
-- that looks (at least syntactically) side-effect free. E.g. a function call is
-- not a read expression, but an array or field access is.
data ExprRead = ExprRead
  { _position   :: AstPosition
  , _identifier :: CExpression SemPhase
  , _varType    :: Type
  } deriving (Show, Eq)
makeFieldsNoPrefix ''ExprRead

currentReads :: (MonadBrowser m) => m [(Ident, Type)]
currentReads = do
  s <- currentStmt
  return $ readsStatement s



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
    readsInitializer (CInitExpr e _)   = readsExpression e
    readsInitializer (CInitList lst _) = concatMap (readsInitializer.snd) lst

    readsDeclaration :: CDeclaration SemPhase -> ([(Ident, Type)], [Ident])
    readsDeclaration (CDecl _ declrs _) = (reads, declared)
      where
        reads        = concatMap readsInitializer initializers :: [(Ident,Type)]
        initializers = mapMaybe (\(_,x,_) -> x) declrs
        declarators  = mapMaybe (\(x,_,_) -> x) declrs :: [CDeclarator SemPhase]
        declared     = mapMaybe identifier' declarators
          where identifier' (CDeclr mi _ _ _ _) = mi
    readsDeclaration _ = ([],[])


-- | find all (sub-expressions) of a statement that
--  * does contain an identifier, and
--  * does not contain a function call
--  * is of integral type
-- Important: Ignores some statements, e.g. compound statements
readExprStatement :: Stmt -> [CExpression SemPhase]
readExprStatement = \case
  (CExpr (Just e) _)  -> readExpression e
  (CIf e _ _ _)       -> readExpression e
  (CWhile e _ _ _)    -> readExpression e
  (CLabel _ stmt _ _) -> readExprStatement stmt
  (CSwitch e _ _)     -> readExpression e
  (CFor (Left me1) me2 me3 _ _) -> concat $ catMaybes $ (fmap.fmap $ readExpression) [me1, me2, me3]
  (CFor (Right (CDecl _ ds _)) me2 me3 _ _) -> let
            reads1 = readExpression $ mapMaybe (\(_,x,_) -> x) ds
            reads2 = concat $ catMaybes $ (fmap.fmap $ readExpression) [me2, me3]
            declared = identifiers $ mapMaybe (\(x,_,_) -> x) ds

          in [ r | r <-  (reads1 ++ reads2), null (identifiers r `intersect` declared) ]
  _                   -> []
  where
    readExpression x =
      [ expr :: CExpression SemPhase
      | expr <- universeBi x
      , isIntegralType (getType expr)
      , not . null $ identifiers expr
      , null $ functionCalls expr
      , null $ assignments expr
      ]
    identifiers e   = [ i :: Ident | i <- universeBi e]
    functionCalls e = [ fn :: CExpression SemPhase | CCall fn _ _ <- universeBi e]
    assignments  e  = [ a  :: CExpression SemPhase | a@(CAssign _ _ _ _) <- universeBi e]



-- first parameter is the number of explored siblings per level (deepest first)
-- TODO: Rewrite this in terms of traverseStmtM
traverseReads :: (MonadBrowser m) => ([(Ident,Type)] -> m ()) -> m ()
traverseReads f = traverseAST' [0]
  where
    traverseAST' st = do
      v <- currentReads
      unless (null v) $ f v
      d <- go Down
      if d
        then do
          (n:_) <- (^. stmtPosition ) <$> getBrowserState
          traverseAST' (n : st)
        else do
          x <- go Next
          if x
            then traverseAST' st
            else ascend st

    ascend [] = return ()
    ascend (n:ns) =
      whenM (go Up) $ do
        replicateM_ n (go_ Next)
        new <- go Next
        if new
          then traverseAST' ns
          else ascend ns



markAllReads :: TU -> TU
markAllReads tu =
  let fnames = map identToString (definedFunctions tu)
      act = forM_ fnames $ \fname -> do
        gotoFunction fname
        traverseReads (insertBefore . Fragments.mkReadMarker)
  in snd <$> runIdentity $ runBrowserT act tu



findAllReads :: TU -> [VarRead]
findAllReads tu = let (_,lg) = runWriter (runBrowserT action tu)
                  in (DL.toList lg)
  where
    action :: BrowserT  (Writer (DL.DList VarRead)) ()
    action = do
      forM_ (definedFunctions tu) $ \f -> do
        gotoFunction (identToString f)
        traverseReads $ \vars -> do
            p <- currentPosition
            forM_ vars $ \(i,t) -> do
              lift $ tell $ DL.singleton $ VarRead p i t

--------------------------------------------------------------------------------
-- subexpression based functions

findAllExprReads :: TU -> [ExprRead]
findAllExprReads tu = let (l,_) = runBrowser action tu
                      in (DL.toList l)
  where
    action :: Browser (DL.DList ExprRead)
    action = do
      res <- forM (definedFunctions tu) $ \f -> do
        gotoFunction (identToString f)
        traverseStmtM $ do
            stmt <- currentStmt
            p <- currentPosition
            let exprs = readExprStatement stmt
            return $ DL.fromList [ExprRead p e (getType e) | e <- exprs]
      return $ mconcat res

markAllExprReads :: TU -> TU
markAllExprReads tu = snd (runBrowser trav tu)
  where trav = traverseStmtsOfTU tu $ do
          subExprs <- readExprStatement <$> currentStmt
          unless (null subExprs) $ insertBefore $ Fragments.mkExprReadMarker subExprs

--------------------------------------------------------------------------------
-- traversal functions

-- | traverses all statements of a translation unit
traverseStmtsOfTU :: (MonadBrowser m, Semigroup w, Monoid w) => TU -> (m w) -> m w
traverseStmtsOfTU tu action = do
  let fnames = map identToString (definedFunctions tu)
  x <- forM fnames $ \fname -> do
    gotoFunction fname
    traverseStmtM action
  return $ mconcat x

-- | traverses the stmt, calling action at every stmt and collecting the results as a monoidal sum
traverseStmtM :: (MonadBrowser m, Semigroup w, Monoid w) => (m w) -> m w
traverseStmtM f = traverseAST' [0]
  where
  traverseAST' st = do
    r <- f
    d <- go Down
    rs <- if d
            then do
            (n:_) <- (^. stmtPosition ) <$> getBrowserState
            traverseAST' (n : st)
            else do
              x <- go Next
              if x
                then traverseAST' st
                else ascend st
    return $ r <> rs

  ascend [] = return mempty
  ascend (n:ns) = do
    u <- go Up
    if u
      then do
        replicateM_ n (go_ Next)
        new <- go Next
        if new
          then traverseAST' ns
          else ascend ns
      else return mempty
