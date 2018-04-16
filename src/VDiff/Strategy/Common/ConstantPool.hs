{-# LANGUAGE MultiWayIf #-}

-- | TODO: Uses nub O(n^2) multiple times.
module VDiff.Strategy.Common.ConstantPool
  ( findAllConstants
  , blurConstants
  , ConstantPool
  , lookupPool
  ) where

import           RIO
import qualified RIO.Map                           as Map

import           Data.Generics.Uniplate.Operations
import           Data.List
import           Language.C
import           Language.C.Analysis.AstAnalysis2
import           Language.C.Analysis.SemRep        hiding (Stmt)
import           Language.C.Analysis.TypeUtils

import           VDiff.Instrumentation

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


displayList :: Display a => [a] -> Utf8Builder
displayList xs = mconcat $ intersperse ", " (map display xs)


blurConstants :: ConstantPool -> ConstantPool
blurConstants (ConstantPool p) = dedupConstants $ ConstantPool $ foldl' f Map.empty (Map.keys p)
  where
    f m ty = Map.insert ty (blur ty (Map.findWithDefault [] ty p)) m

blur :: Type -> [CConstant SemPhase] -> [CConstant SemPhase]
blur ty orig
  | ty `sameType` integral TyInt = concat [ orig
                                          , map (modIntConstant (+1)) orig
                                          , map (modIntConstant (\n -> n - 1)) orig
                                          ]
  | otherwise = orig



modIntConstant :: (Integer -> Integer) -> CConstant a -> CConstant a
modIntConstant f (CIntConst (CInteger v rep flags) ann) = CIntConst (CInteger (f v) rep flags) ann
modIntConstant _ _ = error "incorrect use of modIntConstant"
