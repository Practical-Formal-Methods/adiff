{-# LANGUAGE TemplateHaskell #-}

module Spec.Instrumentation where

import           Data.FileEmbed
import           RIO
import qualified RIO.ByteString                   as BS
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Prelude                          as P

import           Data
import           Types
import           Verifier

import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.TravMonad
import           Language.C.Data.Position
import           Language.C.Parser
import           Language.C.Syntax
import           Text.PrettyPrint                 (render)

import           Control.Monad.State
import           Data.Generics.Uniplate.Data      ()
import           Data.Generics.Uniplate.Zipper
import           Language.C.Data.Lens

import           Instrumentation
import           Zipping

import           Debug.Trace

testInstrumentation = testGroup "instrumentation" [testZipping]

-- | simple structure for zipper
data SimpleState = SimpleState
  { _stmtZipper   :: StmtZipper
  , _siblingIndex :: Int
  }

instance ZipperState SimpleState where
  stmtZipper = lens _stmtZipper (\s z -> s { _stmtZipper = z})
  siblingIndex = lens _siblingIndex (\s i -> s { _siblingIndex = i})

--------------------------------------------------------------------------------

testZipping = testCase "zipping"  $ do
  ast <- parseAndAnalyseFile testFile
  let eds = ast ^. externalDeclarations
      (Just mainDef) = fdef "main" eds
      stmt  = mainDef ^. body

  x <- runStateT walk1 (SimpleState (zipper stmt) 0)
  return ()


  where
    testFile = $(embedFile "assets/test/simple_reads.c")
    walk1 = do
      go_ Down
      go_ Next
      -- v <- findReads
      -- liftIO $ assertBool "find reads" (not (null v))


parseAndAnalyseFile :: ByteString -> IO (CTranslationUnit SemPhase)
parseAndAnalyseFile bs =  do
  case parseC bs (initPos "simple_reads.c") of
    Left err -> assertFailure "simple_reads.c should be parseable"
    Right ast -> do
      case runTrav_ (analyseAST ast) of
        Left typeError  -> assertFailure "simple_reads should be typeable"
        Right (ast', _) -> return ast'

-- for every position in `findAllPositions` insert does not throw an exception,
-- requires a generator for CFiles, or a large corpus TODO


