{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Implements the core instrumentation functions.
module ADiff.Instrumentation
 (
   -- * Handling C files
  openCFile
 , preprocess
 , maskAsserts
 , defineAssert
 , Direction(..)
 , MonadBrowser
 , BrowserT
 , runBrowserT
 , insertBefore
 , buildTranslationUnit
 , tryout
 , go
 , gotoPosition
 , gotoFunction
 , currentStmt
 , currentPosition
 , findCalledFunction
 , go_
 , AstPosition
 , astDepth
 , readStatement
 -- * Internals
 , insertBeforeNthStatement
 -- * mostly for testing
 , markAllReads
 -- * Other
 , ExprRead(..), expression, position
 , findAllReads
 ) where

import           RIO.FilePath
import           Safe
import           ADiff.Prelude

import           Control.Lens.Operators
import           Data.Generics.Uniplate.Data       ()
import           Data.Generics.Uniplate.Operations
import           Data.List                         (isPrefixOf)
import qualified Data.List.Index                   as IL
import           Language.C.Analysis.TravMonad
import           Language.C.Data.Lens
import           Language.C.System.GCC
import           UnliftIO.Directory

import           ADiff.Instrumentation.Browser
import qualified ADiff.Instrumentation.Fragments   as Fragments
import           ADiff.Instrumentation.Reads



-- | short-hand for open, parse and type annotate, will log parse and type checking errors and warnings.
openCFile :: HasLogFunc env => FilePath -> RIO env (Maybe TU)
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

-- | Before we can add assertions to file we have apply a few transformations first.
-- * mask asserts
-- * declare __VERIFIER_error (if not declared)
-- * define __VERIFIER_assert (if not defined)
preprocess :: TU -> TU
preprocess = defineAssert . declareError . maskGotoError. maskAsserts


-- | Returns the name of a function if that function is called at the current statement
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

-- | replaces all @goto ERROR@ statements with the empty statement
maskGotoError = transformBi maskGotoStmt
  where
    maskGotoStmt :: Stmt -> Stmt
    maskGotoStmt stmt@(CGoto n ann)
      | identToString n == "ERROR" = CExpr Nothing ann
      | otherwise = stmt
    maskGotoStmt stmt = stmt

--------------------------------------------------------------------------------
-- | * Masking
--------------------------------------------------------------------------------

-- | replaces all functions calls to __VERIFIER_assert with __DUMMY_VERIFIER_assert.
-- Also replaces all calls to __VERIFIER_error that are not inside the definition of __VERIFIER_assert() with _DUMMY_VERIFIER_assert.
maskAsserts :: TU -> TU
maskAsserts = insertDummy . mask
  where
    insertDummy = insertExtDeclAt 0 (CFDefExt Fragments.dummyError) .
                  insertExtDeclAt 0 (CFDefExt Fragments.dummyAssert)

    mask tu =  snd $ runBrowser (traverseStmtsOfTU tu act) tu
    act = do
      fn <- functionName <$> currentPosition
      unless (fn == "__VERIFIER_assert") $ do
        modifyCurrentStmt $ transformBi replaceFunctionCalls
        modifyCurrentStmt removeErrorLabel

    replaceFunctionCalls :: CExpression SemPhase -> CExpression SemPhase
    replaceFunctionCalls c@(CCall (CVar v ann) e2 ann2)
      | identToString v == "__VERIFIER_assert"  = CCall (CVar (internalIdent "__DUMMY_VERIFIER_assert") ann) e2 ann2
      | identToString v == "__VERIFIER_error"  = CCall (CVar (internalIdent "__DUMMY_VERIFIER_error") ann) e2 ann2
      | otherwise = c
    replaceFunctionCalls c = c

    removeErrorLabel :: Stmt -> Stmt
    removeErrorLabel (CLabel l stmt _ _)
      | identToString l == "ERROR" = stmt
    removeErrorLabel stmt = stmt



-- | Some test cases only use @__VERIFIER_error()@, in those cases we have to define @__VERIFIER_assert()@
-- It's important to insert the definition /after/ the external declaration of @__VERIFIER_error()@.
defineAssert :: TU -> TU
defineAssert tu = case tu ^? ix "__VERIFIER_assert" of
                    Just _  -> tu
                    Nothing ->
                      let (Just p) = indexOfDeclaration "__VERIFIER_error" tu
                      in insertExtDeclAt (p+1) (CFDefExt Fragments.assertDefinition) tu



declareError :: TU ->  TU
declareError tu = case tu ^? ix "__VERIFIER_error" of
                    Just _ -> tu
                    Nothing -> insertExtDeclAt 0 (CDeclExt Fragments.errorDeclaration) tu

--------------------------------------------------------------------------------
-- Small Helpers

insertExtDeclAt  :: Int -> CExternalDeclaration p -> CTranslationUnit p -> CTranslationUnit p
insertExtDeclAt  n d (CTranslUnit exts ann) = CTranslUnit (IL.insertAt n d exts) ann

-- | returns the index of the declaration that declares the given name.
indexOfDeclaration :: String -> TU -> Maybe Int
indexOfDeclaration name (CTranslUnit exts _) = IL.ifindIndex flt exts
  where flt _ d =
          let idents = map identToString $ universeBi d
          in  name `elem` idents

