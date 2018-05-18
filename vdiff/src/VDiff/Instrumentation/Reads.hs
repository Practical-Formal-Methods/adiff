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


-- | A "read expression" is a subexpression that contains read variables and
-- that looks (at least syntactically) side-effect free. E.g. a function call is
-- not a read expression, but an array or field access is.
data ExprRead = ExprRead
  { _position   :: AstPosition
  , _expression :: CExpression SemPhase
  } deriving (Show, Eq)
makeFieldsNoPrefix ''ExprRead


-- | find all (sub-expressions) of a statement that
--  * does contain an identifier, and
--  * does not contain a function call
--  * is of integral type
-- Important: Ignores some statements, e.g. compound statements
readStatement :: SearchMode -> Stmt -> [CExpression SemPhase]
readStatement mode = \case
  (CExpr (Just e) _)  -> readExpression e
  (CIf e _ _ _)       -> readExpression e
  (CWhile e _ _ _)    -> readExpression e
  (CLabel _ stmt _ _) -> readStatement mode stmt
  (CSwitch e _ _)     -> readExpression e
  (CFor (Left me1) me2 me3 _ _) -> concat $ catMaybes $ (fmap.fmap $ readExpression) [me1, me2, me3]
  (CFor (Right (CDecl _ ds _)) me2 me3 _ _) -> let
            reads1 = readExpression $ mapMaybe (\(_,x,_) -> x) ds
            reads2 = concat $ catMaybes $ (fmap.fmap $ readExpression) [me2, me3]
            declared = identifiers $ mapMaybe (\(x,_,_) -> x) ds

          in [ r | r <-  (reads1 ++ reads2), null (identifiers r `intersect` declared) ]
  _                   -> []
  where
    readExpression :: (Data from) => from -> [CExpression SemPhase]
    readExpression x = case mode of
      IdentOnly      -> [ e | e@(CVar _ _ ) <- universeBi x]
      Subexpressions -> subexprs x

    subexprs x =
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



findAllReads :: SearchMode -> TU -> [ExprRead]
findAllReads mode tu = let (l,_) = runBrowser action tu
                      in (DL.toList l)
  where
    action :: Browser (DL.DList ExprRead)
    action = do
      res <- forM (definedFunctions tu) $ \f -> do
        gotoFunction (identToString f)
        traverseStmtM $ do
            stmt <- currentStmt
            p <- currentPosition
            let exprs = readStatement mode stmt
            return $ DL.fromList [ExprRead p e | e <- exprs]
      return $ mconcat res

markAllReads :: SearchMode ->  TU -> TU
markAllReads mode tu = snd (runBrowser trav tu)
  where trav = traverseStmtsOfTU tu $ do
          subExprs <- readStatement mode <$> currentStmt
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
