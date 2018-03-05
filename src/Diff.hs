{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Diff where

import           Prelude                    hiding (fail, reads)

import           Control.Exception
import           Control.Monad              hiding (fail)
import           Control.Monad.Trans.Reader
import           Data.List                  (nub, sortBy)
import           Data.Monoid                ((<>))
import           Data.Ord                   (comparing)

import           Language.C
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Random
import           Text.PrettyPrint           (render)

import           Types
import           Verifiers


-- TODO: this uses naive strategy, always
diff :: MainParameters -> IO ()
diff CmdVersions = cmdVersions
diff p = do
  res <- parseCFilePre (program p)
  case res of
    Left err        -> putStrLn $ show err
    Right cTrUnit -> do
      initRandom
      putStrLn "successfully parsed"
      let genPos = translationUnit cTrUnit
          posN = length $ runGen genPos
      putStrLn ("insertion points: " ++ show posN)
      forM_ (runGen genPos) $ \inserter -> do
        j <- randomRIO (-100,100) :: IO Integer
        let asTmpl = notEqualsAssertion j
            mutated = runReader inserter asTmpl

        withSystemTempFile "input.c" $ \fp h -> do
          let rendered = render $ pretty mutated
          hPutStr h rendered
          hFlush h
          verResults <- mapM (`execute` fp) (verifiers p)
          when (length (nub verResults) > 1) $ do
            putStrLn "found inconsistency with the following file:"
            putStrLn rendered
            exitSuccess

        return ()

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
  mkAssertion :: Ident -> CStat
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
translationUnit :: CTranslUnit -> Gen AssertionTemplate CTranslUnit
translationUnit (CTranslUnit eds a)= do
  eds' <- branch extDeclaration eds
  return $ CTranslUnit eds' a


extDeclaration :: CExtDecl -> Gen AssertionTemplate CExtDecl
extDeclaration (CDeclExt _ ) = fail
extDeclaration (CAsmExt _ _) = fail
extDeclaration (CFDefExt f)  =  case functionName f of
  Just ('_' : ('_' : _)) -> fail -- ignore things that start with two underscores
  _                      -> CFDefExt <$> functionDefinition f

functionName :: CFunctionDef a -> Maybe String
functionName (CFunDef _ (CDeclr mIdent _ _ _ _) _ _ _) = identToString <$> mIdent

functionDefinition :: CFunDef -> Gen AssertionTemplate CFunDef
functionDefinition (CFunDef specs declr decla stmt x) = CFunDef specs declr decla <$> statement stmt <*> pure x

-- | usually just identity unless it is a compound statement.
statement :: CStat -> Gen AssertionTemplate CStat
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


insertAssert :: [CCompoundBlockItem NodeInfo]
             -> Gen AssertionTemplate [CCompoundBlockItem NodeInfo]

insertAssert []                            = fail
insertAssert (f@(CNestedFunDef _) : items) = (f:) <$> insertAssert items
insertAssert (f@(CBlockDecl _) : items)    = (f:) <$> insertAssert items
insertAssert (b@(CBlockStmt stmt) : items) = here  <> further
  where here = MkGen [reader $ \r ->  (CBlockStmt $ mkAssertion r v ) : b : items| v <- reads stmt]
        further = (b:)  <$> insertAssert items




cCompoundBlockItem :: CCompoundBlockItem NodeInfo -> Gen AssertionTemplate (CCompoundBlockItem NodeInfo)
cCompoundBlockItem (CBlockStmt stmt ) = CBlockStmt <$> statement stmt
cCompoundBlockItem (CBlockDecl _)     = fail
cCompoundBlockItem (CNestedFunDef _)  = fail



--------------------------------------------------------------------------------
-- provisional stuff
class HasReads a where
  reads :: a -> [Ident]

instance HasReads (CStatement a) where
  reads (CExpr (Just e) _)  = reads e
  reads (CExpr Nothing _)   = []
  reads (CIf e _ _ _)       = reads e -- [internalIdent "x"]
  reads (CWhile e _ _ _)    = reads e
  reads (CLabel _ stmt _ _) = reads stmt
  reads _                   = []

instance HasReads (CExpression a) where
  reads (CVar n _)        = [n]
  reads (CBinary _ l r _) = reads l <> reads r
  reads (CUnary _ e _)    = reads e
  reads _                 = []


notEqualsAssertion :: Integer -> AssertionTemplate
notEqualsAssertion i = AssertionTemplate $ \varName ->
  let identifier = CVar (builtinIdent "__VERIFIER_assert") undefNode
      expression = CBinary CNeqOp  (CVar varName undefNode) (CConst (CIntConst (cInteger i) undefNode )) undefNode
  in CExpr (Just $ CCall identifier [expression]  undefNode) undefNode
