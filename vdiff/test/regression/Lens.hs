module Lens where

import           VDiff.Prelude
import           Util

import           Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as LC8

import           Language.C.Data.Lens
import           VDiff.Instrumentation

testLenses :: IO TestTree
testLenses = testGroup "lenses" <$> sequence [ pure testIndex
                                             , testDefinedFunctions
                                             ]

testIndex :: TestTree
testIndex = goldenVsString "index main" "assets/test/lenses/functions.c.golden" act
  where act = do
          (Just ast) <- runRIO NoLogging $ openCFile defaultTypechecker "assets/test/lenses/functions.c"
          let dummyBody = CCompound [] [CBlockStmt (dummyStmt "dummy")] (undefNode, voidType)
              ast' = (ix "main" . functionDefinition . body ) .~ dummyBody $ ast
          return $ LC8.pack $ prettyp ast'

testDefinedFunctions :: IO TestTree
testDefinedFunctions = testGroup "defined functions" . map runTest <$> findByExtension [".c"] "assets/test/reads"
  where
    runTest cf = vsGoldenFile cf "defined-functions" $ return . LC8.pack . show . map identToString . definedFunctions

