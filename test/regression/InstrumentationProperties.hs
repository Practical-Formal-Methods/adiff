{-# LANGUAGE TemplateHaskell #-}

module InstrumentationProperties where

import           RIO

import           Control.Lens
import           Data.FileEmbed
import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Test.Tasty.Hedgehog

import           Language.C.Data.Lens
import           Util
import           VDiff.Instrumentation

--------------------------------------------------------------------------------
-- properties
--------------------------------------------------------------------------------

testInstrumentationProperties :: TestTree
testInstrumentationProperties = testGroup "properties"
  [
    testRandomWalk
  , testInsert
  , testGoto
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


deriving instance MonadIO (BrowserT (PropertyT IO))

-- test property: goto current position should not change anything

testGoto :: TestTree
testGoto = testGroup "goto" [testGoto1, testGoto2]

testGoto1 :: TestTree
testGoto1 = testProperty "(goto . currentPosition) does not modify location" $ property $ do
  stmt <- genTestStmts "assets/test/reads"
  ds <- forAll genDirectionList
  let actn = do
        mapM_ go ds
        x <- currentStmt
        p <- currentPosition
        goto p
        y <- currentStmt
        return (x,y)
  ((x,y), _) <- runBrowserT actn stmt
  prettyp x === prettyp y

testGoto2 :: TestTree
testGoto2 = testProperty "with random positions" $ property $ do
  stmt <- genTestStmts "assets/test/reads"
  (p,s) <- forAll $ genPosition stmt
  let actn = do
        goto p
        currentStmt
  (s', _) <- runBrowserT actn stmt
  prettyp s === prettyp s'



-- | traverses the tree randomly
genPosition :: Stmt ->  Gen (AstPosition, Stmt)
genPosition stmt = do
  ds <- genDirectionList
  let actn = do
        mapM_ go ds
        p <- currentPosition
        s <- currentStmt
        return (p,s)
  (x, _) <- runBrowserT actn stmt
  return x




controlReads :: ByteString
controlReads= $(embedFile "assets/test/reads/control.c")
