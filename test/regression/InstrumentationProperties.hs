{-# LANGUAGE TemplateHaskell #-}

module InstrumentationProperties where

import           RIO hiding ((^.))

import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad.State
import           Data.FileEmbed
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Tasty.Hedgehog

import           Util

import Debug.Trace

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
  let preZipper = mkZipper stmt
  (_,st) <- runStateT walk (SimpleState preZipper [0])
  let postZipper = (st ^. stmtZipper)
  prettyp (fromZipper preZipper) === prettyp (fromZipper postZipper)
  return ()


debugState :: (MonadState st m, ZipperState st) => m ()
debugState = do
  si <- use stmtPosition
  x <- currentStmt
  traceM $ "ZZZ: sibling index = " ++ show si ++ ", current stmt = " ++ prettyp x
  return ()

-- -- Conclusion: Up does not go back to the first element. 
-- -- Down goes to the first element, though?
-- weirdBehaviour :: TestTree
-- weirdBehaviour = testCase "weird behaviour " $ do
--   (Just tu) <- runRIO NoLogging $ openCFile "assets/test/reads/three-ifs.c"
--   let (Just stmt) =  tu ^? (ix "main" . functionDefinition . body)
--   let walk = do
--         go Down
--         go Next
--         insertBefore $ dummyStmt "dummy-01"
--         debugState
--         go Next
--         insertBefore $ dummyStmt "dummy-02"
--         debugState
        
--   (_,st) <- runStateT walk (SimpleState (mkZipper stmt) [0])
--   traceM $ prettyp $ fromZipper (st ^. stmtZipper)
--   return ()
--   -- assertEqual "up.down = id" (prettyp x) (prettyp y)

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
  ((x,y),_) <- runStateT actn (SimpleState (mkZipper stmt) [0])
  prettyp x === prettyp y


controlReads :: ByteString
controlReads= $(embedFile "assets/test/reads/control.c")
