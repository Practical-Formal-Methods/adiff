{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

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
 , BrowserT
 , runBrowserT
 , currentReads
 , insertBefore
 , buildTranslationUnit
 , tryout
 , go
 , gotoPosition
 , gotoFunction
 , currentStmt
 , currentPosition
 , findCalledFunction
 , findReads
 , go_
 , AstPosition
 -- * Internals
 , insertBeforeNthStatement
 , markAllReads
 ) where

import           RIO
import           RIO.FilePath
import           Safe

-- import           Control.Lens
import           Control.Monad.Writer              hiding ((<>))
import qualified Data.DList                        as DL
import           Data.Functor.Identity
import           Data.Generics.Uniplate.Data       ()
import           Data.Generics.Uniplate.Operations
import           Data.List                         (isPrefixOf)
import           Data.Text                         (pack)
import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.SemRep        hiding (Stmt)
import           Language.C.Analysis.TravMonad
import           Language.C.Analysis.TypeUtils
import           Language.C.Data.Lens
import           Language.C.System.GCC
import           Text.PrettyPrint                  (render)
import           UnliftIO.Directory

import           VDiff.Instrumentation.Browser


instance Display (CStatement a) where
  display = display . pack . prettyp

prettyp :: Pretty a => a -> String
prettyp = render . Language.C.pretty

-- | short-hand for open, parse and type annotate, will log parse and type checking errors and warnings.
openCFile :: HasLogFunc env => FilePath -> RIO env (Maybe (CTranslationUnit SemPhase))
openCFile fn = do
  -- we need GCC to remove preprocessor tokens and comments,
  -- unfortunately, GCC only works on files with .c ending. Hence this hack.
  let templateName = takeFileName $ replaceExtension fn ".c"
  withSystemTempFile  templateName $  \fnC _ ->  do
    copyFile fn fnC
    x <- liftIO $ parseCFile (newGCC "gcc") Nothing [] fnC
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
        declared     = mapMaybe identifier declarators
          where identifier (CDeclr mi _ _ _ _) = mi
    readsDeclaration _ = ([],[])








currentReads :: (MonadBrowser m) => m [(Ident, Type)]
currentReads = do
  s <- currentStmt
  return $ readsStatement s

findReads :: CTranslationUnit SemPhase -> [(AstPosition, Ident, Type)]
findReads tu = let (x,y) = runBrowserT findReads' tu
               in x

findReads' :: (MonadBrowser m) => m [(AstPosition, Ident, Type)]
findReads' = (DL.toList . snd) <$> runWriterT action
  where
    action = traverseReads $ \vars -> do
            p <- currentPosition
            forM_ vars $ \(i,t) -> tell $ DL.singleton (p,i,t)




markAllReads :: CTranslationUnit SemPhase-> CTranslationUnit SemPhase
markAllReads tu =
  let fnames = map identToString (definedFunctions tu)
      act = forM_ fnames $ \fname -> do
        gotoFunction fname
        traverseReads (insertBefore . mkReadMarker)
  in snd <$> runIdentity $ runBrowserT act tu


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

findCalledFunction :: (MonadBrowser m) => m (Maybe String)
findCalledFunction = do
  stmt <- currentStmt
  let subExprs = universeBi stmt :: [CExpression SemPhase]
  let fns = [ n
            | CCall (CVar i _) _ _ <- subExprs
            , let n = identToString i
            , not ("__" `isPrefixOf` n)
            ]
  return $ headMay fns

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


