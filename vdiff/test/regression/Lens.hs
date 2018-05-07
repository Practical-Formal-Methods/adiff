module Lens where

import           RIO
import           Util

import           Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as LC8

import           Language.C.Data.Lens
import           VDiff.Instrumentation
import           VDiff.Types

testLenses :: IO TestTree
testLenses = do
  children <- sequence [ pure testIndex
                 , testDefinedFunctions
                 ]
  return $ testGroup "lenses" children

testIndex :: TestTree
testIndex = goldenVsString "index main" "assets/test/lenses/functions.c.golden" act
  where act = do
          (Just ast) <- runRIO NoLogging $ openCFile "assets/test/lenses/functions.c"
          let dummyBody = CCompound [] [CBlockStmt (dummyStmt "dummy")] (undefNode, voidType)
              ast' = (ix "main" . functionDefinition . body ) .~ dummyBody $ ast
          return $ LC8.pack $ prettyp ast'

testDefinedFunctions :: IO TestTree
testDefinedFunctions = do
  cFiles <- findByExtension [".c"] "assets/test/reads"
  return $ testGroup "defined functions" $ map runTest cFiles
  where runTest cf = vsGoldenFile cf "defined-functions" $ \ast -> do
              let names =  map identToString $ definedFunctions ast
              return $ LC8.pack $ show names

