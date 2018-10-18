-- MIT License
--
-- Copyright (c) 2018 Christian Klinger
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE TemplateHaskell #-}

module InstrumentationProperties where

import           ADiff.Prelude

import           Control.Lens
import           Data.FileEmbed
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog

import           Language.C.Data.Lens
import           Util
import           ADiff.Instrumentation
import           ADiff.Instrumentation.Browser

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
genTestStmts dir = do
  tu <- genTranslationUnit dir
  let (Just stmt) = tu ^? (ix "main" . functionDefinition . body)
  return stmt

genTranslationUnit :: FilePath -> PropertyT IO (CTranslationUnit SemPhase)
genTranslationUnit dir = do
  cFiles <- lift $ findByExtension [".c"] dir
  cFile <- forAll $ Gen.element cFiles
  (Just tu) <- runRIO NoLogging $ openCFile cFile
  return tu

-- | test property: Any walk does not change the file
testRandomWalk ::  TestTree
testRandomWalk = testProperty "walk does not modify" $ property $ do
  ds <- forAll genDirectionList
  tu <- genTranslationUnit "assets/test/reads"
  let walk = mapM go ds
  (_,tu') <- runBrowserT walk tu
  prettyp tu === prettyp tu'


-- TODO: test property: After an arbitrary walk and inserting one element, "currentStatement" is the same.
testInsert :: TestTree
testInsert = testProperty "insertBefore, does not modify location" $ property $ do
  tu <- genTranslationUnit "assets/test/reads"
  ds <- forAll genDirectionList
  let actn = do
        mapM_ go ds
        x <- currentStmt
        let exprs = readStatement IdentOnly x
        unless (null exprs) $ insertBefore (dummyStmt "dummy")
        y <- currentStmt
        return (x,y)
  ((x,y),_) <- runBrowserT actn tu
  prettyp x === prettyp y


deriving instance MonadIO (BrowserT (PropertyT IO))

-- test property: goto current position should not change anything

testGoto :: TestTree
testGoto = testGroup "goto" [testGoto1, testGoto2]

testGoto1 :: TestTree
testGoto1 = testProperty "(goto . currentPosition) does not modify location" $ property $ do
  tu <- genTranslationUnit "assets/test/reads"
  ds <- forAll genDirectionList
  let actn = do
        mapM_ go ds
        x <- currentStmt
        p <- currentPosition
        gotoPosition p
        y <- currentStmt
        return (x,y)
  ((x,y), _) <- runBrowserT actn tu
  prettyp x === prettyp y

testGoto2 :: TestTree
testGoto2 = testProperty "with random positions" $ property $ do
  tu <- genTranslationUnit "assets/test/reads"
  (p,s) <- forAll $ genPosition tu
  let actn = do
        gotoPosition p
        currentStmt
  (s', _) <- runBrowserT actn tu
  prettyp s === prettyp s'



-- | traverses the tree randomly
genPosition :: CTranslationUnit SemPhase->  Gen (AstPosition, Stmt)
genPosition tu = do
  ds <- genDirectionList
  let actn = do
        mapM_ go ds
        p <- currentPosition
        s <- currentStmt
        return (p,s)
  (x, _) <- runBrowserT actn tu
  return x




controlReads :: ByteString
controlReads= $(embedOneFileOf [ "assets/test/reads/control.c"
                               , "adiff/assets/test/reads/control.c"
                               ])
