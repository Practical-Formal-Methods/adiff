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

{-# LANGUAGE MultiWayIf #-}

-- | TODO: Uses nub O(n^2) multiple times.
module ADiff.Strategy.Common.ConstantPool
  ( findAllConstants
  , blurConstants
  , ConstantPool
  , lookupPool
  ) where

import           ADiff.Prelude
import qualified RIO.Map                           as Map

import           Data.Generics.Uniplate.Data       ()
import           Data.Generics.Uniplate.Operations
import           Data.List
import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.SemRep        hiding (Stmt)
import           Language.C.Analysis.TypeUtils


newtype ConstantPool = ConstantPool (Map Type [CConstant SemPhase])



instance Display ConstantPool where
  display (ConstantPool p) = Map.foldMapWithKey f p
    where
      f _ cs = "[ " <> displayList (map (tshow.prettyp) cs) <> "]"


findAllConstants :: CTranslationUnit SemPhase -> ConstantPool
findAllConstants tu = dedupConstants $ ConstantPool $ foldl' f Map.empty (universeBi tu)
  where
    f m c = Map.insertWith (++) (getType c) [c] m

dedupConstants :: ConstantPool -> ConstantPool
dedupConstants (ConstantPool p) = ConstantPool $ fmap nub' p
  where nub' = nubBy (\x y -> pretty x == pretty y )
  -- TODO: equality over pretty-printing does not sound smart


lookupPool :: Type -> ConstantPool -> [CConstant SemPhase]
lookupPool ty (ConstantPool p) = Map.findWithDefault [] ty p



blurConstants :: ConstantPool -> ConstantPool
blurConstants (ConstantPool p) = dedupConstants $ ConstantPool $ foldl' f Map.empty (Map.keys p)
  where
    f m ty = Map.insert ty (blur ty (Map.findWithDefault [] ty p)) m

blur :: Type -> [CConstant SemPhase] -> [CConstant SemPhase]
blur ty orig
  | ty `sameType` integral TyInt = concat [ orig
                                          , map (modIntConstant (+1)) orig
                                          , map (modIntConstant (\n -> n - 1)) orig
                                          , map (modIntConstant (const minInt)) orig
                                          , map (modIntConstant (const maxInt)) orig
                                          ]
  | otherwise = orig

minInt, maxInt :: Integer
minInt = fromIntegral (minBound :: Int32)
maxInt = fromIntegral (maxBound :: Int32)

modIntConstant :: (Integer -> Integer) -> CConstant a -> CConstant a
modIntConstant f (CIntConst (CInteger v rep flags) ann) = CIntConst (CInteger (f v) rep flags) ann
modIntConstant _ _ = error "incorrect use of modIntConstant"
