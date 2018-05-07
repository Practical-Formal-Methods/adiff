{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module VDiff.Instrumentation.Reads where

import           RIO

import           Control.Monad.Writer            hiding ((<>))
import qualified Data.DList                      as DL
import           Data.Functor.Identity
import           VDiff.Instrumentation.Browser
import qualified VDiff.Instrumentation.Fragments as Fragments
import           VDiff.Types

data VarRead = VarRead
  { _position   :: AstPosition
  , _identifier :: Ident
  , _varType    :: Type
  } deriving (Show, Eq)
makeFieldsNoPrefix ''VarRead


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




-- first parameter is the number of explored siblings per level (deepest first)
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

markAllReads :: TU -> CTranslationUnit SemPhase
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
