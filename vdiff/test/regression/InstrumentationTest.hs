module InstrumentationTest where

import qualified Data.ByteString.Lazy.Char8  as LC8
import           RIO
import qualified RIO.ByteString.Lazy         as LBS
import           System.FilePath             (replaceExtension, takeBaseName)

import           Language.C
import           Text.PrettyPrint            (render)

import           Control.Lens.Operators      ((^?))
import           Data.Generics.Uniplate.Data ()
import           Language.C.Data.Lens

import           VDiff.Instrumentation

import           Util



testInstrumentation :: IO TestTree
testInstrumentation = do
  tests <- sequence [testZipping]
  return $ testGroup "instrumentation" tests



--------------------------------------------------------------------------------
testZipping :: IO TestTree
testZipping = do
  tests <- sequence
    [ pure testWalk
    , pure testInsertions
    , pure testInsertBefore
    , testMarkAllReads
    , pure testPreprocessor
    ]
  return $ testGroup "zipping" tests


testWalk :: TestTree
testWalk = testCase "walks"  $ do
  ast <- parseAndAnalyseFile simpleReads
  _ <- runBrowserT walk1 ast
  return ()


  where
    walk1 = do
      gotoFunction "main"
      go_ Down
      findReads >>= \v -> assertBool' "find x " (varNames v == ["x"])
      go_ Down
      go_ Down --into the compound statement
      findReads >>= \v -> assertBool' "find nothing " (null $ varNames v)
      go_ Next
      findReads >>= \v -> assertBool' "find y" (varNames v  == ["y"])
      go_ Next
      findReads >>= \v -> assertBool' "find nothing " (null $ varNames v)
      go_ Next
      findReads >>= \v -> assertBool' "find x" (varNames v  == ["x"])
      go_ Next
      findReads >>= \v -> assertBool' "find z" (varNames v  == ["z"])
      return ()



testInsertions :: TestTree
testInsertions = vsGoldenFile "assets/test/instrumentation/simple.c" "insertion" $ \ast -> do
  (_,st) <- runBrowserT inserter ast
  return $ LC8.pack $ prettyp st
  where
    inserter = do
      gotoFunction "main"
      go_ Down
      go_ Down
      go_ Down -- now
      insertBefore $ dummyStmt "dummy_01"
      go_ Next
      insertBefore $ dummyStmt "dummy_02"
      go_ Next
      insertBefore $ dummyStmt "dummy_03"
      return ()

testPreprocessor :: TestTree
testPreprocessor = testGroup "gcc" [ vsGoldenFile "assets/test/gcc/1.c" "preprocess1" pp
                                   , vsGoldenFile "assets/test/gcc/2.i" "preprocess2" pp
                                   ]
  where
    pp ast = return $ LC8.pack $ prettyp ast


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


testMarkAllReads :: IO TestTree
testMarkAllReads = do
  cFiles <- findByExtension [".c"] "assets/test/reads"
  return $ testGroup "markAllReads golden tests" $ map runTest cFiles
  where
    runTest cf = vsGoldenFile cf "all-reads"  $ \tu -> do
        let bs = render . pretty . markAllReads $ tu
        return $ LC8.pack bs

