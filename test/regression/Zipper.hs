module Zipper where

import RIO

import qualified Data.ByteString.Lazy.Char8       as LC8
import           RIO
import qualified RIO.ByteString.Lazy              as LBS
import           System.FilePath                  (replaceExtension,
                                                   takeBaseName)

import           Language.C
import           Text.PrettyPrint                 (render)

import           Control.Lens.Operators           ((^?))
import           Data.Generics.Uniplate.Data      ()
import           Instrumentation
import           Language.C.Data.Lens

import           Util

testZipper = testGroup "zippers" []

testRandomMoves  = undefined
