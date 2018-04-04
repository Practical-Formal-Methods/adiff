{-# LANGUAGE TemplateHaskell #-}

module InstrumentationProperties where

import           RIO

import           Control.Lens
import           Data.FileEmbed
import           Hedgehog
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import           Test.Tasty.Hedgehog

import           Util
import           Instrumentation
import           Language.C.Data.Lens

--------------------------------------------------------------------------------
-- properties
--------------------------------------------------------------------------------

testInstrumentationProperties :: TestTree
testInstrumentationProperties = testGroup "properties"
  [
    testRandomWalk
  , testInsert
  ]


genDirectionList :: Gen [Direction]
genDirectionList =
  let listLength = Range.linear 0 30
  in  Gen.list listLength Gen.enumBounded


-- TODO: Extend from "main" to all functions
genTestStmts :: FilePath -> PropertyT IO Stmt
genTestStmts fp = do
  cFiles <- lift $ findByExtension [".c"] fp
  cFile <- forAll $ Gen.element cFiles
  (Just tu) <- runRIO NoLogging $ openCFile cFile
  let (Just stmt) = tu ^? (ix "main" . functionDefinition . body)
  return stmt

-- | test property: Any walk does not change the file
testRandomWalk ::  TestTree
testRandomWalk = testProperty "walk does not modify" $ property $ do
  ds <- forAll genDirectionList
  stmt <- genTestStmts "assets/test/reads"
  let walk = mapM go ds
  (_,stmt') <- runBrowserT walk stmt
  prettyp stmt === prettyp stmt'


-- TODO: test property: After an arbitrary walk and inserting one element, "currentStatement" is the same.
testInsert :: TestTree
testInsert = testProperty "insertBefore, does not modify location" $ property $ do
  stmt <- genTestStmts "assets/test/reads"
  ds <- forAll genDirectionList
  let actn = do
        mapM_ go ds
        x <- currentStmt
        vs <- findReads
        unless (null vs) $ insertBefore (dummyStmt "dummy")
        y <- currentStmt
        return (x,y)
  ((x,y),_) <- runBrowserT actn stmt
  prettyp x === prettyp y


controlReads :: ByteString
controlReads= $(embedFile "assets/test/reads/control.c")
