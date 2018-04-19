module Lens where

import           RIO
import           Util

import           Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as LC8

import           Language.C.Data.Lens
import           VDiff.Instrumentation

testLenses :: TestTree
testLenses = testGroup "lenses" [testIndex]

testIndex :: TestTree
testIndex = goldenVsString "index main" "assets/test/lenses/functions.c.golden" act
  where act = do
          (Just ast) <- runRIO NoLogging $ openCFile "assets/test/lenses/functions.c"
          let dummyBody = CCompound [] [CBlockStmt (dummyStmt "dummy")] (undefNode, voidType)
              ast' = (ix "main" . functionDefinition . body ) .~ dummyBody $ ast
          return $ LC8.pack $ prettyp ast'