module Spec.Lens where

import           RIO
import           Spec.Util

import           Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as LC8

import           Instrumentation
import           Language.C.Data.Lens

testLenses :: TestTree
testLenses = testGroup "lenses" [testIndex]

testIndex :: TestTree
testIndex = goldenVsString "index main" "assets/test/lenses/functions.c.golden" act
  where act = do
          ast <- runRIO NoLogging $ openCFile "assets/test/lenses/functions.c"
          let dummyBody = CCompound [] [CBlockStmt (dummyStmt "dummy")] (undefNode, voidType)
              ast' = (ix "main" . functionDefinition . body ) .~ dummyBody $ ast
          return $ LC8.pack $ prettyp ast'
