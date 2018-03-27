{-# LANGUAGE Rank2Types #-}
-- | provides some lenses that makes working with the AST more bearable
module Language.C.Data.Lens where

import qualified Prelude    as P
import           RIO

import           Language.C




-- TODO: Is that a traversal?
fdef :: String -> [CExternalDeclaration a] -> Maybe (CFunctionDef a)
fdef fn = P.foldl flt Nothing
  where
    flt (Just d) _                                = Just d
    flt Nothing (CFDefExt fd@(CFunDef _ (CDeclr (Just i) _ _ _ _) _ _ _)) =
      if identToString i == fn
      then Just fd
      else Nothing
    flt _ _ = Nothing


body :: Lens' (CFunctionDef a) (CStatement a)
body = lens getter setter
  where getter (CFunDef _ _ _ s _) = s
        setter (CFunDef specs declr decls _ ann) b' = CFunDef specs declr decls b' ann

externalDeclarations :: Lens' (CTranslationUnit a) [CExternalDeclaration a]
externalDeclarations = lens getter setter
  where
    getter (CTranslUnit eds _) = eds
    setter (CTranslUnit _ ann) eds = CTranslUnit eds ann
