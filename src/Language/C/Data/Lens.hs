{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
-- | provides some lenses that makes working with the AST more bearable
module Language.C.Data.Lens
  ( externalDeclarations
  , functionDefinition
  , declarator
  , body
  , module Control.Lens.At
  , module Control.Lens.Traversal
  ) where

import           RIO

import           Control.Lens.At
import           Control.Lens.Prism
import           Control.Lens.Traversal

import           Language.C


body :: Lens' (CFunctionDef a) (CStatement a)
body = lens getter setter
  where getter (CFunDef _ _ _ s _) = s
        setter (CFunDef specs declr decls _ ann) b' = CFunDef specs declr decls b' ann

externalDeclarations :: Lens' (CTranslationUnit a) [CExternalDeclaration a]
externalDeclarations = lens getter setter
  where
    getter (CTranslUnit eds _) = eds
    setter (CTranslUnit _ ann) eds = CTranslUnit eds ann


type instance Index (CTranslationUnit a) = String
type instance IxValue (CTranslationUnit a) = CExternalDeclaration a

-- instance At (CTranslationUnit a)

class HasDeclarator e a where
  declarator :: Lens' e (CDeclarator a)

instance HasDeclarator (CFunctionDef a) a where
  declarator = lens getter setter
    where
      getter (CFunDef _ d _ _ _ ) = d
      setter (CFunDef specs _ decls b ann) d = CFunDef specs d decls b ann


functionDefinition :: Prism' (CExternalDeclaration a) (CFunctionDef a)
functionDefinition = prism' CFDefExt project
  where project (CFDefExt f) = Just f
        project _            = Nothing

instance Ixed (CTranslationUnit a) where
  ix :: String -> Traversal' (CTranslationUnit a) (CExternalDeclaration a)
  ix str = externalDeclarations . traverse'
    where
      traverse' :: (Applicative f) => (CExternalDeclaration a -> f (CExternalDeclaration a)) -> [CExternalDeclaration a] -> f [CExternalDeclaration a]
      traverse' act []                   = pure []
      traverse' act (x@(CDeclExt _ ):xs) = (x:) <$> traverse' act xs -- TODO: not checking those here yet
      traverse' act (x@(CFDefExt f@(CFunDef _ (CDeclr (Just i) _ _ _ _) _ _ _) ):xs)
        | identToString i == str         = (:) <$> act x  <*> traverse' act xs
        | otherwise                      = (x:) <$> traverse' act xs
      traverse' act (x:xs) = (x:) <$> traverse' act xs

