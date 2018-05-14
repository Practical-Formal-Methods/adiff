{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | provides some lenses that makes working with the AST more bearable
module Language.C.Data.Lens
  ( externalDeclarations
  , functionDefinition
  , externalDeclaration
  , declarator
  , body
  , definedFunctions
  , HasIdent(ident)
  , module Control.Lens.At
  , module Control.Lens.Traversal
  ) where

import           RIO                    hiding (Lens', lens, (^.))

import           Control.Lens
import           Control.Lens.At
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

externalDeclaration :: Prism' (CExternalDeclaration a) (CDeclaration a)
externalDeclaration = prism' CDeclExt project
  where project (CDeclExt d) = Just d
        project _            = Nothing

class HasIdent e where
  ident :: Traversal' e Ident


instance HasIdent (CDeclarator a) where
  ident f (CDeclr (Just i) declr ms attrs ann) = CDeclr <$> (Just <$> f i) <*> pure declr <*> pure ms <*> pure attrs <*> pure ann
  ident _ d@(CDeclr Nothing _ _ _ _)           = pure d

instance Ixed (CTranslationUnit a) where
  ix :: String -> Traversal' (CTranslationUnit a) (CExternalDeclaration a)
  ix str = externalDeclarations . traverse'
    where
      traverse' :: (Applicative f) => (CExternalDeclaration a -> f (CExternalDeclaration a)) -> [CExternalDeclaration a] -> f [CExternalDeclaration a]
      traverse' _ []                   = pure []
      -- traverse' act (x@(CDeclExt _ ):xs) = (x:) <$> traverse' act xs
      traverse' act (x@(CDeclExt (CDecl _ [(Just declr, _, _)] _) ):xs) =
        case declr ^? ident of
          Just ident
            | identToString ident == str -> (:) <$> act x <*> traverse' act xs
          _ -> (x:) <$> traverse' act xs

      traverse' act (x@(CFDefExt (CFunDef _ (CDeclr (Just i) _ _ _ _) _ _ _) ):xs)
        | identToString i == str         = (:) <$> act x  <*> traverse' act xs
        | otherwise                      = (x:) <$> traverse' act xs
      traverse' act (x:xs) = (x:) <$> traverse' act xs


-- this should be a lens one-liner, but I don't know how, so just a function for now
definedFunctions :: CTranslationUnit SemPhase -> [Ident]
definedFunctions tu =
  let exts =  tu ^. externalDeclarations
      fdefs = foldl' f [] exts
        where f l ext =  case ext ^? functionDefinition of
                           Nothing -> l
                           Just fd -> fd : l
      idents = foldl' g [] fdefs
        where
          g l (CFunDef _ declr _ _ _) = case declr ^? ident of
                                          Nothing -> l
                                          Just i  -> i:l
  in idents
