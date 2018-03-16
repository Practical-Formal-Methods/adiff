{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving#-}


module Diff where

import           Prelude                    hiding (fail, reads)

import           Control.Exception
import           Control.Monad              hiding (fail)
import           Control.Monad.Trans.Reader
import           Data.List                  (nub, sortBy)
import           Data.Monoid                ((<>))
import           Data.Ord                   (comparing)

import           Language.C
import Language.C.Analysis.TypeUtils
import Language.C.Analysis.SemRep
import Language.C.Analysis.AstAnalysis2
import Language.C.Analysis.TravMonad
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Random
import           Text.PrettyPrint           (render)


import Debug.Trace

import           Types
import           Verifier


-- short-hand for open, parse and type annotate
openCFile :: FilePath -> IO (CTranslationUnit SemPhase)
openCFile fn = parseCFilePre fn >>= \case
    Left parseError -> do
      hPutStrLn stderr "parse error: "
      hPrint stderr parseError
      exitFailure
    Right tu -> case runTrav_ (analyseAST tu) of
        Left typeError -> do
          hPutStrLn stderr "type error: "
          hPrint stderr typeError
          exitFailure
        Right (tu', warnings) -> do
          unless (null warnings) $ do
            putStrLn "warnings: "
            hPrint stderr warnings
          return tu'


-- TODO: this uses naive strategy, always
diff :: MainParameters -> IO ()
diff (CmdParseTest fn) = cmdParseTest fn
diff CmdVersions = cmdVersions
diff p = do
  ast <- openCFile (program p)
  -- initRandom
  putStrLn "successfully parsed"
  let genPos = translationUnit ast
      posN = length $ runGen genPos
  putStrLn ("insertion points: " ++ show posN)
  forM_ (runGen genPos) $ \inserter -> do
    j <- randomRIO (-10000,100000) :: IO Integer
    let asTmpl = notEqualsAssertion j
        mutated = runReader inserter asTmpl

    withSystemTempFile "input.c" $ \fp h -> do
      let rendered = render $ pretty mutated
      hPutStr h rendered
      hFlush h
      verResults <- mapM (`execute` fp) (verifiers p)
      when (length (nub verResults) > 1) $ do
        putStrLn "found inconsistency:"
        mapM_ putStrLn $ zipWith (\v r-> verifierName v ++ " = " ++ show r ) (verifiers p) verResults
        putStrLn "with file:"
        putStrLn rendered
        exitSuccess

  return ()

-- | pares the file, runs the semantic analysis (type checking), and pretty-prints the resulting semantic AST.
-- Use this to test the modified language-c-extensible library.
cmdParseTest :: FilePath -> IO ()
cmdParseTest fn = openCFile fn >>= putStrLn . render . pretty

cmdVersions :: IO ()
cmdVersions = forM_ (sortBy (comparing verifierName) allVerifiers) $ \verifier -> do
    putStr $ verifierName verifier
    putStr ": "
    sv <- try (version verifier) >>= \case
      Left (_ :: IOException) -> return "unknown (error)"
      Right Nothing -> return "unknown"
      Right (Just v) -> return v
    putStrLn sv


-- TODO: check if seed is in the arguments
initRandom :: IO ()
initRandom = do
      seed <- randomIO :: IO Int
      let rnd = mkStdGen seed
      putStrLn $ "seed: " <> show seed
      setStdGen rnd

--------------------------------------------------------------------------------
-- * Helpers

-- | A position is actually a function from the thing to be inserted to a new TranslationUnit

newtype AssertionTemplate = AssertionTemplate {
  mkAssertion :: (Ident, Type) -> CStatement SemPhase
  }

newtype Gen i a = MkGen { runGen :: [Reader i a] }
  deriving (Functor, Monoid)

instance Applicative (Gen i) where
  pure x = MkGen [ pure x]
  fs <*> as = MkGen [reader $ \r -> runReader f r (runReader a r) | f <- runGen fs, a <- runGen as]


-- | Use this when it was not possible to insert an asset in this part of the program
fail :: Gen i a
fail = MkGen []

-- Therefore we need @branch@
branch :: (a -> Gen r a) -> [a] -> Gen r [a]
branch _ []     = fail
branch f (b:bs) = here <> later
  where here =  (:) <$> f b <*> pure bs
        later = (:) <$> pure b <*> branch f bs

--------------------------------------------------------------------------------
-- * Generators
--------------------------------------------------------------------------------
translationUnit :: CTranslationUnit SemPhase -> Gen AssertionTemplate (CTranslationUnit SemPhase)
translationUnit (CTranslUnit eds a)= do
  eds' <- branch extDeclaration eds
  return $ CTranslUnit eds' a


extDeclaration :: CExternalDeclaration SemPhase -> Gen AssertionTemplate (CExternalDeclaration SemPhase)
extDeclaration (CDeclExt _ ) = fail
extDeclaration (CAsmExt _ _) = fail
extDeclaration (CFDefExt f)  =  case functionName f of
  Just ('_' : ('_' : _)) -> fail -- ignore things that start with two underscores
  _                      -> CFDefExt <$> functionDefinition f

functionName :: CFunctionDef SemPhase -> Maybe String
functionName (CFunDef _ (CDeclr mIdent _ _ _ _) _ _ _) = identToString <$> mIdent

functionDefinition :: CFunctionDef SemPhase -> Gen AssertionTemplate (CFunctionDef SemPhase)
functionDefinition (CFunDef specs declr decla stmt x) = CFunDef specs declr decla <$> statement stmt <*> pure x

-- | usually just identity unless it is a compound statement.
statement :: CStatement SemPhase -> Gen AssertionTemplate (CStatement SemPhase)
statement (CLabel i stmt attrs a)      = CLabel i <$> statement stmt <*> pure attrs <*> pure a
statement (CCase e stmt a)             = CCase e <$> statement stmt <*> pure a
statement (CCases e1 e2 stmt a)        = CCases e1 e2 <$> statement stmt <*> pure a
statement (CDefault stmt a)            = CDefault <$> statement stmt <*> pure a
statement (CCompound ids blkItems ann) = here <> deeper -- this order is bfs, otherwise it's dfs
  where
    here                               = CCompound ids <$> insertAssert blkItems <*> pure ann
    deeper                             = CCompound ids <$> branch cCompoundBlockItem  blkItems <*> pure ann
statement (CIf e th Nothing a)         = CIf e <$> statement th <*> pure Nothing <*> pure a
statement (CIf e th (Just el) a)       = (CIf e <$> statement th <*> pure Nothing <*> pure a) <>
                                         (CIf e <$> pure th <*> (Just <$> statement el) <*> pure a)
statement (CSwitch e stmt a)           = CSwitch e <$> statement stmt <*> pure a
statement (CWhile e s b a)             = CWhile e <$> statement s <*> pure b <*> pure a
statement (CFor hd me1 me2 stmt a)     = CFor hd me1 me2 <$> statement stmt <*> pure a
statement (CExpr _ _)                  = fail
statement (CGoto _ _)                  = fail
statement (CGotoPtr _ _)               = fail
statement (CCont _)                    = fail
statement (CBreak _)                   = fail
statement (CReturn _ _)                = fail
statement (CAsm _ _)                   = fail


insertAssert :: [CCompoundBlockItem SemPhase]
             -> Gen AssertionTemplate [CCompoundBlockItem SemPhase]

insertAssert []                            = fail
insertAssert (f@(CNestedFunDef _) : items) = (f:) <$> insertAssert items
insertAssert (f@(CBlockDecl _) : items)    = (f:) <$> insertAssert items
insertAssert (b@(CBlockStmt stmt) : items) = here  <> further
  where here = MkGen [reader $ \r ->  (CBlockStmt $ mkAssertion r v ) : b : items| v <- reads stmt] :: Gen AssertionTemplate [CCompoundBlockItem SemPhase]
        further = (b:)  <$> insertAssert items :: Gen AssertionTemplate [CCompoundBlockItem SemPhase]




cCompoundBlockItem :: CCompoundBlockItem SemPhase -> Gen AssertionTemplate (CCompoundBlockItem SemPhase)
cCompoundBlockItem (CBlockStmt stmt ) = CBlockStmt <$> statement stmt
cCompoundBlockItem (CBlockDecl _)     = fail
cCompoundBlockItem (CNestedFunDef _)  = fail



--------------------------------------------------------------------------------
-- provisional stuff
class HasReads a where
  reads :: a -> [(Ident, Type)]

instance HasReads (CStatement SemPhase) where
  reads (CExpr (Just e) _)  = reads e
  reads (CExpr Nothing _)   = []
  reads (CIf e _ _ _)       = reads e -- [internalIdent "x"]
  reads (CWhile e _ _ _)    = reads e
  reads (CLabel _ stmt _ _) = reads stmt
  reads _                   = []

instance HasReads (CExpression SemPhase) where
  reads (CVar n (_,ty))        = [(n, ty)]
  reads (CBinary _ l r _) = reads l <> reads r
  reads (CUnary _ e _)    = reads e
  reads _                 = []


notEqualsAssertion :: Integer -> AssertionTemplate
notEqualsAssertion i = AssertionTemplate $ \(varName,ty) ->
  let identifier = CVar (builtinIdent "__VERIFIER_assert") (undefNode,voidType)
      var = CVar varName (undefNode, ty) :: CExpression SemPhase
      const'
        | ty `sameType` integral TyChar = CConst (CCharConst (CChar (toEnum $ fromIntegral $ i `mod` 256) False) (undefNode, integral TyChar))
        | ty `sameType` boolType        = CConst (CIntConst (cInteger (i `mod` 2)) (undefNode, boolType))
        | otherwise                     = trace ("don't know how to handle this type: " ++ ("get show instances for types")) $ CConst (CIntConst (cInteger i) (undefNode, simpleIntType ))
      expression = CBinary CNeqOp var const' (undefNode, boolType) :: CExpression SemPhase
  in CExpr (Just $ CCall identifier [expression]  (undefNode,voidType)) (undefNode,voidType)

simpleIntType :: Type
simpleIntType = integral (getIntType noFlags)
