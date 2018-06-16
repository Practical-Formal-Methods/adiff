{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module VDiff.Instrumentation.Reads where

import           VDiff.Prelude

import           Control.Monad.Writer              hiding ((<>))
import qualified Data.DList                        as DL
import           Data.Functor.Identity
import           Data.Generics.Uniplate.Operations
import           Data.List                         (intersect, isPrefixOf)
import           Language.C.Analysis.TypeUtils     (isIntegralType)
import           VDiff.Instrumentation.Browser
import qualified VDiff.Instrumentation.Fragments   as Fragments


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
            reads1 = concatMap readExpression $  universeBi $ mapMaybe (\(_,x,_) -> x) ds
            reads2 = concat $ catMaybes $ (fmap.fmap $ readExpression) [me2, me3]
            declared = identifiers $ mapMaybe (\(x,_,_) -> x) ds

          in [ r | r <-  (reads1 ++ reads2), null (identifiers r `intersect` declared) ]
  _                   -> []
  where
    readExpression :: CExpression SemPhase-> [CExpression SemPhase]
    readExpression x = case mode of
      IdentOnly      -> nub prettyp [ e | e@(CVar _ _ ) <- universeBi $ collect x]
      Subexpressions -> subexprs x

    subexprs x =
      [ expr :: CExpression SemPhase
      | expr <- collect x
      , isIntegralType (getType expr)
      , not . null $ identifiers expr
      , null $ functionCalls expr
      , null $ assignments expr
      , null $ incOrDecs expr
      ]

    identifiers e   = [ i :: Ident | i <- universeBi e]
    functionCalls e = [ fn :: CExpression SemPhase | CCall fn _ _ <- universeBi e]
    assignments  e  = [ a  :: CExpression SemPhase | a@(CAssign _ _ _ _) <- universeBi e]
    incOrDecs e     = [ x :: CExpression SemPhase | x@(CUnary op _ _) <- universeBi e, incOrDec op]

    -- basically like universe :: Expr -> Expr, but only goes down the left side of an assign statement
    collect e@(CVar _ _)        = [e]
    collect e@(CUnary _ e1 _ )  = e : collect e1
    collect e@(CBinary _ l r _) = e : (collect l <> collect r)
    collect (CAssign _ _ e2 _)  = collect e2
    collect (CCall _ es _)      = concatMap collect es
    collect e@(CIndex _ e2 _)   = e : collect e2
    collect _                   = []

    incOrDec CPreIncOp  = True
    incOrDec CPreDecOp  = True
    incOrDec CPostIncOp = True
    incOrDec CPostDecOp = True
    incOrDec _          = False


findAllReads :: SearchMode -> TU -> [ExprRead]
findAllReads mode tu = let (l,_) = runBrowser action tu
                      in (DL.toList l)
  where
    action :: Browser (DL.DList ExprRead)
    action = do
      let functions = filter (\f -> not ("__" `isPrefixOf` (identToString f))) $ definedFunctions tu
      res <- forM functions $ \f -> do
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

