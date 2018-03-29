{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Spec.Instrumentation where

import qualified Data.ByteString.Lazy.Char8       as LC8
import           Data.FileEmbed
import           RIO
import qualified RIO.ByteString.Lazy              as LBS
import           System.FilePath                  (replaceExtension,
                                                   takeBaseName)
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Diff

import qualified Prelude                          as P


import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.TravMonad
import           Language.C.Analysis.TypeUtils
import           Text.PrettyPrint                 (render)

import           Control.Monad.State
import           Data.Generics.Uniplate.Data      ()
import           Language.C.Data.Lens

import           Instrumentation

testInstrumentation :: IO TestTree
testInstrumentation = do
  tests <- sequence [testZipping]
  return $ testGroup "instrumentation" tests


-- | simple structure for zipper
data SimpleState = SimpleState
  { _stmtZipper   :: StmtZipper
  , _siblingIndex :: Int
  }

instance ZipperState SimpleState where
  stmtZipper = lens _stmtZipper (\s z -> s { _stmtZipper = z})
  siblingIndex = lens _siblingIndex (\s i -> s { _siblingIndex = i})

--------------------------------------------------------------------------------
testZipping :: IO TestTree
testZipping = do
  tests <- sequence
    [ pure testWalk
    , pure testInsertions
    , pure testInsertBefore
    , testMarkAllReads
    ]
  return $ testGroup "zipping" tests

testWalk :: TestTree
testWalk = testCase "walks"  $ do
  ast <- parseAndAnalyseFile simpleReads
  let eds = ast ^. externalDeclarations
      (Just mainDef) = fdef "main" eds
      stmt  = mainDef ^. body

  x <- runStateT walk1 (SimpleState (mkZipper stmt) 0)
  return ()


  where
    walk1 = do
      go_ Down
      findReads >>= \v -> assertBool' "find x " (varNames v == ["x"])
      go_ Down
      go_ Down --into the compound statement
      findReads >>= \v -> assertBool' "find nothing " (varNames v == [])
      go_ Next
      findReads >>= \v -> assertBool' "find y" (varNames v  == ["y"])
      go_ Next
      findReads >>= \v -> assertBool' "find nothing " (varNames v == [])
      go_ Next
      findReads >>= \v -> assertBool' "find x" (varNames v  == ["x"])
      go_ Next
      findReads >>= \v -> assertBool' "find z" (varNames v  == ["z"])
      return ()



testInsertions = testCase "insertions"  $ do
  ast <- parseAndAnalyseFile simpleReads
  let eds = ast ^. externalDeclarations
      (Just mainDef) = fdef "main" eds
      stmt  = mainDef ^. body
  (_,st) <- runStateT inserter (SimpleState (mkZipper stmt) 0)
  P.putStrLn "final result"
  print' $ fromZipper (st ^. stmtZipper)
  return ()


  where
    inserter = do
      go_ Down
      go_ Down
      go_ Down -- now
      insertBefore $ dummyStmt "dummy_01"
      go_ Next
      insertBefore $ dummyStmt "dummy_02"
      go_ Next
      insertBefore $ dummyStmt "dummy_03"
      -- go_ Next
      -- printCurrentStmt
      -- insertBefore dummyStmt
      -- go_ Next
      -- insertBefore dummyStmt
      return ()

--------------------------------------------------------------------------------

testInsertBefore :: TestTree
testInsertBefore = testGroup "insertBefore" [one]
  where
    one = testCase "insert at 0" $ do
      let itemA =  CBlockStmt $ dummyStmt "a"
          itemB = CBlockStmt $ dummyStmt "b"
          itemC = CBlockDecl dummyDecl
          stmt   = dummyStmt "xx"
      let items1 = insertBeforeNthStatement stmt 0 [itemA, itemB, itemC]
      assertEqual "after insertion" (show [CBlockStmt stmt, itemA, itemB, itemC]) (show items1)
      let items2 = insertBeforeNthStatement stmt 1 [itemA, itemB, itemC]
      assertEqual "after insertion" (show [itemA, CBlockStmt stmt, itemB, itemC]) (show items2)

data NoLogging = NoLogging
instance HasLogFunc NoLogging where
  logFuncL = lens getter setter
    where logFunc = mkLogFunc (\_ _ _ _ -> return ())
          getter = const logFunc
          setter = const $ const NoLogging

testMarkAllReads :: IO TestTree
testMarkAllReads = do
  cFiles <- findByExtension [".c"] "assets/test/reads"
  return $ testGroup "markAllReads golden tests"
      [ goldenVsString bn gold (mar cFile)
      | cFile <- cFiles
      , let gold = replaceExtension cFile ".golden"
      , let bn = takeBaseName cFile
      ]

  where
    mar :: String -> IO LBS.ByteString
    mar fn = do
      tu <- runRIO NoLogging $ openCFile fn
      let bs = render . pretty . markAllReads $ tu
      return $ LC8.pack bs



--------------------------------------------------------------------------------
-- properties
--------------------------------------------------------------------------------
-- TODO: test property: Any walk does not change the file

--------------------------------------------------------------------------------
-- ** Little Helpers
--------------------------------------------------------------------------------



dummyDecl :: CDeclaration SemPhase
dummyDecl = CDecl [] [] undefNode

dummyStmt :: String -> Stmt
dummyStmt s = CExpr (Just var) (undefNode, voidType)
  where var = CVar (internalIdent s) (undefNode, voidType)

assertBool' :: (MonadIO m) => String -> Bool -> m ()
assertBool' s b = liftIO $ assertBool s b


varNames :: [(Ident,b)] -> [String]
varNames = map (identToString.fst)

print' = P.putStrLn . render . pretty

simpleReads = $(embedFile "assets/test/reads/simple_reads.c")

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


