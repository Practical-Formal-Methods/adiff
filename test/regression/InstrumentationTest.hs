{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module InstrumentationTest where

import qualified Data.ByteString.Lazy.Char8       as LC8
import           Data.FileEmbed
import           RIO
import qualified RIO.ByteString.Lazy              as LBS
import           System.FilePath                  (replaceExtension,
                                                   takeBaseName)

import           Language.C
import           Text.PrettyPrint                 (render)

import           Control.Lens.Operators           ((^?))
import           Control.Monad.State
import           Data.Generics.Uniplate.Data      ()
import           Instrumentation
import           Language.C.Data.Lens

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
    ]
  return $ testGroup "zipping" tests

testWalk :: TestTree
testWalk = testCase "walks"  $ do
  ast <- parseAndAnalyseFile simpleReads
  let (Just stmt) = ast ^? (ix "main" . functionDefinition . body)
  _ <- runStateT walk1 (SimpleState (mkZipper stmt) 0)
  return ()


  where
    walk1 = do
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
  let (Just stmt ) = ast ^? (ix "main" . functionDefinition . body)
  (_,st) <- runStateT inserter (SimpleState (mkZipper stmt) 0)
  return $ LC8.pack $ prettyp $ fromZipper (st ^. stmtZipper)
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
      (Just tu) <- runRIO NoLogging $ openCFile fn
      let bs = render . pretty . markAllReads $ tu
      return $ LC8.pack bs





