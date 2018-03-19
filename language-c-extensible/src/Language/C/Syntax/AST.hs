{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax.AST
-- Copyright   :  (c) [1999..2007] Manuel M T Chakravarty
--                (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  : benedikt.huber@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Abstract syntax of C source and header files.
--
--  The tree structure is based on the grammar in Appendix A of K&R.  The
--  abstract syntax simplifies the concrete syntax by merging similar concrete
--  constructs into a single type of abstract tree structure: declarations are
--  merged with structure declarations, parameter declarations and type names,
--  and declarators are merged with abstract declarators.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988. The AST supports all
--  of C99 <http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf> and several
--  GNU extensions <http://gcc.gnu.org/onlinedocs/gcc/C-Extensions.html>.
-----------------------------------------------------------------------------
module Language.C.Syntax.AST (
  -- * C translation units
  CTranslUnit,  CExtDecl,
  CTranslationUnit(..),  CExternalDeclaration(..),
  -- * Declarations
  CFunDef,  CDecl, CStructUnion, CEnum,
  CFunctionDef(..),  CDeclaration(..),
  CStructTag(..), CStructureUnion(..),  CEnumeration(..),
  -- * Declaration attributes
  CDeclSpec, partitionDeclSpecs,
  CStorageSpec, CTypeSpec, isSUEDef, CTypeQual, CFunSpec, CAlignSpec,  CAttr,
  CFunctionSpecifier(..), CDeclarationSpecifier(..), CStorageSpecifier(..), CTypeSpecifier(..),
  CAlignmentSpecifier(..),
  CTypeQualifier(..), CAttribute(..),
  -- * Declarators
  CDeclr,CDerivedDeclr,CArrSize,
  CDeclarator(..), CDerivedDeclarator(..), CArraySize(..),
  -- * Initialization
  CInit, CInitList, CDesignator,
  CInitializer(..), CInitializerList, CPartDesignator(..),
  -- * Statements
  CStat, CBlockItem, CAsmStmt, CAsmOperand,
  CStatement(..), CCompoundBlockItem(..),
  CAssemblyStatement(..), CAssemblyOperand(..),

  -- * Expressions
  CExpr, CExpression(..),
  CAssignOp(..), CBinaryOp(..), CUnaryOp(..),
  CBuiltin, CBuiltinThing(..),
  -- * Constants
  CConst, CStrLit, cstringOfLit, liftStrLit,
  CConstant(..), CStringLiteral(..),

  -- * Annotations
  AnnTranslationUnit,
  AnnDeclaration,
  AnnFunctionDef,
  AnnMisc,
  AnnExpression,
  AnnInitializer,
  AnnAsmExt,
  AnnStringLiteral,
  AnnStatement,
  AnnConstant,
) where
import           Control.DeepSeq             (NFData)
import           Data.Generics               hiding (Generic)
import           GHC.Generics                (Generic, Generic1)
import           Language.C.Data.Ident
import           Language.C.Data.Node
import           Language.C.Data.Position
import           Language.C.Syntax.Constants
import           Language.C.Syntax.Ops


type family AnnTranslationUnit x
type family AnnDeclaration x
type family AnnFunctionDef x
type family AnnMisc x
type family AnnExpression x
type family AnnInitializer x
type family AnnAsmExt x
type family AnnStringLiteral x
type family AnnStatement x
type family AnnConstant x

-- for backwards-compat
type instance AnnTranslationUnit NodeInfo = NodeInfo
type instance AnnFunctionDef NodeInfo     = NodeInfo
type instance AnnExpression NodeInfo      = NodeInfo
type instance AnnInitializer NodeInfo     = NodeInfo
type instance AnnStringLiteral NodeInfo   = NodeInfo
type instance AnnAsmExt NodeInfo          = NodeInfo
type instance AnnStatement NodeInfo       = NodeInfo
type instance AnnConstant NodeInfo        = NodeInfo
type instance AnnMisc NodeInfo            = NodeInfo -- I'm lazy, so right now a lot of AST elements just use this index

-- Data instances have to be standalone because they can't work for arbitrary parameters and there's no simple way to add a sensible constraint.
-- (see discussion on the GHC wiki)
deriving instance Data (CAlignmentSpecifier NodeInfo)
deriving instance Data (CArraySize NodeInfo)
deriving instance Data (CAssemblyOperand NodeInfo)
deriving instance Data (CAssemblyStatement NodeInfo)
deriving instance Data (CAttribute NodeInfo)
deriving instance Data (CBuiltinThing NodeInfo)
deriving instance Data (CCompoundBlockItem NodeInfo)
deriving instance Data (CDeclaration NodeInfo)
deriving instance Data (CDeclarationSpecifier NodeInfo)
deriving instance Data (CDeclarator NodeInfo)
deriving instance Data (CDerivedDeclarator NodeInfo)
deriving instance Data (CEnumeration NodeInfo)
deriving instance Data (CExpression NodeInfo)
deriving instance Data (CExternalDeclaration NodeInfo)
deriving instance Data (CFunctionDef NodeInfo)
deriving instance Data (CInitializer NodeInfo)
deriving instance Data (CPartDesignator NodeInfo)
deriving instance Data (CStatement NodeInfo)
deriving instance Data (CStructureUnion NodeInfo)
deriving instance Data (CTypeQualifier NodeInfo)
deriving instance Data (CTypeSpecifier NodeInfo)
deriving instance Data (CStringLiteral NodeInfo)
deriving instance Data (CStorageSpecifier NodeInfo)
deriving instance Data (CConstant NodeInfo)
deriving instance Data (CFunctionSpecifier NodeInfo)


deriving instance Show (CAlignmentSpecifier NodeInfo)
deriving instance Show (CArraySize NodeInfo)
deriving instance Show (CAssemblyOperand NodeInfo)
deriving instance Show (CAssemblyStatement NodeInfo)
deriving instance Show (CAttribute NodeInfo)
deriving instance Show (CBuiltinThing NodeInfo)
deriving instance Show (CCompoundBlockItem NodeInfo)
deriving instance Show (CDeclaration NodeInfo)
deriving instance Show (CDeclarationSpecifier NodeInfo)
deriving instance Show (CDeclarator NodeInfo)
deriving instance Show (CDerivedDeclarator NodeInfo)
deriving instance Show (CEnumeration NodeInfo)
deriving instance Show (CExpression NodeInfo)
deriving instance Show (CExternalDeclaration NodeInfo)
deriving instance Show (CFunctionDef NodeInfo)
deriving instance Show (CInitializer NodeInfo)
deriving instance Show (CPartDesignator NodeInfo)
deriving instance Show (CStatement NodeInfo)
deriving instance Show (CStructureUnion NodeInfo)
deriving instance Show (CTypeQualifier NodeInfo)
deriving instance Show (CTypeSpecifier NodeInfo)
deriving instance Show (CStringLiteral NodeInfo)
deriving instance Show (CStorageSpecifier NodeInfo)
deriving instance Show (CConstant NodeInfo)
deriving instance Show (CFunctionSpecifier NodeInfo)



-- | Complete C tranlsation unit (C99 6.9, K&R A10)
--
-- A complete C translation unit, for example representing a C header or source file.
-- It consists of a list of external (i.e. toplevel) declarations.
type CTranslUnit = CTranslationUnit NodeInfo
data CTranslationUnit a
  = CTranslUnit [CExternalDeclaration a] (AnnTranslationUnit a)
    deriving (Typeable, Generic {-! ,CNode ,Functor, Annotated !-})


-- | External C declaration (C99 6.9, K&R A10)
--
-- Either a toplevel declaration, function definition or external assembler.
type CExtDecl = CExternalDeclaration NodeInfo
data CExternalDeclaration a
  = CDeclExt (CDeclaration a)
  | CFDefExt (CFunctionDef a)
  | CAsmExt  (CStringLiteral a) (AnnAsmExt a)
    deriving (Typeable, Generic {-! ,CNode ,Functor, Annotated !-})


-- | C function definition (C99 6.9.1, K&R A10.1)
--
-- A function definition is of the form @CFunDef specifiers declarator decllist? stmt@.
--
-- * @specifiers@ are the type and storage-class specifiers of the function.
--   The only storage-class specifiers allowed are /extern/ and /static/.
--
-- * The @declarator@ must be such that the declared identifier has /function type/.
--   The return type shall be void or an object type other than array type.
--
-- * The optional declaration list @decllist@ is for old-style function declarations.
--
-- * The statement @stmt@ is a compound statement.
type CFunDef = CFunctionDef NodeInfo
data CFunctionDef a
  = CFunDef
    [CDeclarationSpecifier a] -- type specifier and qualifier
    (CDeclarator a)           -- declarator
    [CDeclaration a]          -- optional declaration list
    (CStatement a)            -- compound statement
    (AnnFunctionDef a)
    deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})



-- | C declarations (K&R A8, C99 6.7), including structure declarations, parameter
--   declarations and type names.
--
-- A declaration is of the form @CDecl specifiers init-declarator-list@, where the form of the declarator list's
--  elements depends on the kind of declaration:
--
-- 1) Toplevel declarations (K&R A8, C99 6.7 declaration)
--
--   * C99 requires that there is at least one specifier, though this is merely a syntactic restriction
--
--   * at most one storage class specifier is allowed per declaration
--
--   * the elements of the non-empty @init-declarator-list@ are of the form @(Just declr, init?, Nothing)@.
--      The declarator @declr@ has to be present and non-abstract and the initialization expression is
--      optional.
--
-- 2) Structure declarations (K&R A8.3, C99 6.7.2.1 struct-declaration)
--
--   Those are the declarations of a structure's members.
--
--   * do not allow storage specifiers
--
--   * in strict C99, the list of declarators has to be non-empty
--
--   * the elements of @init-declarator-list@ are either of the form @(Just declr, Nothing, size?)@,
--     representing a member with optional bit-field size, or of the form @(Nothing, Nothing, Just size)@,
--     for unnamed bitfields. @declr@ has to be non-abstract.
--
--   * no member of a structure shall have incomplete type
--
-- 3) Parameter declarations (K&R A8.6.3, C99 6.7.5 parameter-declaration)
--
--   * @init-declarator-list@ must contain at most one triple of the form @(Just declr, Nothing, Nothing)@,
--     i.e. consist of a single declarator, which is allowed to be abstract (i.e. unnamed).
--
-- 4) Type names (A8.8, C99 6.7.6)
--
--   * do not allow storage specifiers
--
--   * @init-declarator-list@ must contain at most one triple of the form @(Just declr, Nothing, Nothing)@.
--     where @declr@ is an abstract declarator (i.e. doesn't contain a declared identifier)
--
type CDecl = CDeclaration NodeInfo
data CDeclaration a
  = CDecl
    [CDeclarationSpecifier a] -- type specifier and qualifier, __attribute__
    [(Maybe (CDeclarator a),  -- declarator (may be omitted)
      Maybe (CInitializer a), -- optional initialize
      Maybe (CExpression a))] -- optional size (const expr)
    (AnnMisc a)                         -- annotation
    | CStaticAssert
      (CExpression a)         -- assert expression
      (CStringLiteral a)      -- assert text
      (AnnMisc a)                       -- annotation
    deriving (Typeable, Generic {-! ,CNode ,Annotated !-})



-- | C declarator (K&R A8.5, C99 6.7.5) and abstract declarator (K&R A8.8, C99 6.7.6)
--
-- A declarator declares a single object, function, or type. It is always associated with
-- a declaration ('CDecl'), which specifies the declaration's type and the additional storage qualifiers and
-- attributes, which apply to the declared object.
--
-- A declarator is of the form @CDeclr name? indirections asm-name? attrs _@, where
-- @name@ is the name of the declared object (missing for abstract declarators),
-- @declquals@ is a set of additional declaration specifiers,
-- @asm-name@ is the optional assembler name and attributes is a set of
-- attrs is a set of @__attribute__@ annotations for the declared object.
--
-- @indirections@ is a set of pointer, array and function declarators, which modify the type of the declared object as
-- described below. If the /declaration/ specifies the non-derived type @T@,
-- and we have @indirections = [D1, D2, ..., Dn]@ than the declared object has type
-- @(D1 `indirect` (D2 `indirect` ...  (Dn `indirect` T)))@, where
--
--  * @(CPtrDeclr attrs) `indirect` T@ is /attributed pointer to T/
--
--  * @(CFunDeclr attrs) `indirect` T@ is /attributed function returning T/
--
--  * @(CArrayDeclr attrs) `indirect` T@ is /attributed array of elemements of type T/
--
-- Examples (simplified attributes):
--
--  * /x/ is an int
--
-- > int x;
-- > CDeclr "x" []
--
--  * /x/ is a restrict pointer to a const pointer to int
--
-- > const int * const * restrict x;
-- > CDeclr "x" [CPtrDeclr [restrict], CPtrDeclr [const]]
--
--  * /f/ is an function return a constant pointer to int
--
-- > int* const f();
-- > CDeclr "f" [CFunDeclr [],CPtrDeclr [const]]
--
--  * /f/ is a constant pointer to a function returning int
--
-- > int (* const f)(); ==>
-- > CDeclr "f" [CPtrDeclr [const], CFunDeclr []]
type CDeclr = CDeclarator NodeInfo
data CDeclarator a
  = CDeclr (Maybe Ident) [CDerivedDeclarator a] (Maybe (CStringLiteral a)) [CAttribute a] (AnnMisc a)
    deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})



-- | Derived declarators, see 'CDeclr'
--
-- Indirections are qualified using type-qualifiers and generic attributes, and additionally
--
--    * The size of an array is either a constant expression, variable length ('*') or missing; in the last case, the
--      type of the array is incomplete. The qualifier static is allowed for function arguments only, indicating that
--      the supplied argument is an array of at least the given size.
--
--    * New style parameter lists have the form @Right (declarations, isVariadic)@, old style parameter lists have the
--      form @Left (parameter-names)@
type CDerivedDeclr = CDerivedDeclarator NodeInfo
data CDerivedDeclarator a
  = CPtrDeclr [CTypeQualifier a] (AnnMisc a)
  -- ^ Pointer declarator @CPtrDeclr tyquals declr@
  | CArrDeclr [CTypeQualifier a] (CArraySize a) (AnnMisc a)
  -- ^ Array declarator @CArrDeclr declr tyquals size-expr?@
  | CFunDeclr (Either [Ident] ([CDeclaration a],Bool)) [CAttribute a] (AnnMisc a)
    -- ^ Function declarator @CFunDeclr declr (old-style-params | new-style-params) c-attrs@
    deriving (Typeable, Generic {-! ,CNode , Annotated !-})



-- | Size of an array
type CArrSize = CArraySize NodeInfo
data CArraySize a
  = CNoArrSize Bool               -- ^ @CUnknownSize isCompleteType@
  | CArrSize Bool (CExpression a) -- ^ @CArrSize isStatic expr@
    deriving (Typeable, Generic, Generic1 {-! , Functor !-})


-- | C statement (K&R A9, C99 6.8)
--
type CStat = CStatement NodeInfo
data CStatement a
  -- | An (attributed) label followed by a statement
  = CLabel  Ident (CStatement a) [CAttribute a] (AnnStatement a)
  -- | A statement of the form @case expr : stmt@
  | CCase (CExpression a) (CStatement a) (AnnStatement a)
  -- | A case range of the form @case lower ... upper : stmt@
  | CCases (CExpression a) (CExpression a) (CStatement a) (AnnStatement a)
  -- | The default case @default : stmt@
  | CDefault (CStatement a) (AnnStatement a)
  -- | A simple statement, that is in C: evaluating an expression with
  --   side-effects and discarding the result.
  | CExpr (Maybe (CExpression a)) (AnnStatement a)
  -- | compound statement @CCompound localLabels blockItems at@
  | CCompound [Ident] [CCompoundBlockItem a] (AnnStatement a)
  -- | conditional statement @CIf ifExpr thenStmt maybeElseStmt at@
  | CIf (CExpression a) (CStatement a) (Maybe (CStatement a)) (AnnStatement a)
  -- | switch statement @CSwitch selectorExpr switchStmt@, where
  -- @switchStmt@ usually includes /case/, /break/ and /default/
  -- statements
  | CSwitch (CExpression a) (CStatement a) (AnnStatement a)
  -- | while or do-while statement @CWhile guard stmt isDoWhile at@
  | CWhile (CExpression a) (CStatement a) Bool (AnnStatement a)
  -- | for statement @CFor init expr-2 expr-3 stmt@, where @init@ is
  -- either a declaration or initializing expression
  | CFor (Either (Maybe (CExpression a)) (CDeclaration a))
    (Maybe (CExpression a))
    (Maybe (CExpression a))
    (CStatement a)
    (AnnStatement a)
  -- | goto statement @CGoto label@
  | CGoto Ident (AnnStatement a)
  -- | computed goto @CGotoPtr labelExpr@
  | CGotoPtr (CExpression a) (AnnStatement a)
  -- | continue statement
  | CCont (AnnStatement a)
  -- | break statement
  | CBreak (AnnStatement a)
  -- | return statement @CReturn returnExpr@
  | CReturn (Maybe (CExpression a)) (AnnStatement a)
  -- | assembly statement
  | CAsm (CAssemblyStatement a) (AnnStatement a)
    deriving (Typeable, Generic {-! , CNode , Annotated !-})



-- | GNU Assembler statement
--
-- > CAssemblyStatement type-qual? asm-expr out-ops in-ops clobbers _
--
-- is an inline assembler statement.
-- The only type-qualifier (if any) allowed is /volatile/.
-- @asm-expr@ is the actual assembler epxression (a string), @out-ops@ and @in-ops@ are the input
-- and output operands of the statement.
-- @clobbers@ is a list of registers which are clobbered when executing the assembler statement
type CAsmStmt = CAssemblyStatement NodeInfo
data CAssemblyStatement a
  = CAsmStmt
    (Maybe (CTypeQualifier a)) -- maybe volatile
    (CStringLiteral a)         -- assembler expression (String)
    [CAssemblyOperand a]       -- output operands
    [CAssemblyOperand a]       -- input operands
    [CStringLiteral a]         -- Clobbers
    a
    deriving (Typeable, Generic, Generic1 {-! ,CNode ,Functor ,Annotated !-})


-- | Assembler operand
--
-- @CAsmOperand argName? constraintExpr arg@ specifies an operand for an assembler
-- statement.
type CAsmOperand = CAssemblyOperand NodeInfo
data CAssemblyOperand a
  = CAsmOperand
    (Maybe Ident)       -- argument name
    (CStringLiteral a)  -- constraint expr
    (CExpression a)     -- argument
    a
    deriving (Typeable, Generic, Generic1 {-! ,CNode ,Functor ,Annotated !-})


-- | C99 Block items
--
--  Things that may appear in compound statements: either statements, declarations
--   or nested function definitions.
type CBlockItem = CCompoundBlockItem NodeInfo
data CCompoundBlockItem a
  = CBlockStmt    (CStatement a)    -- ^ A statement
  | CBlockDecl    (CDeclaration a)  -- ^ A local declaration
  | CNestedFunDef (CFunctionDef a)  -- ^ A nested function (GNU C)
    deriving (Typeable, Generic, Generic1 {-! , CNode , Functor, Annotated !-})


-- | C declaration specifiers and qualifiers
--
-- Declaration specifiers include at most one storage-class specifier (C99 6.7.1),
-- type specifiers (6.7.2) and type qualifiers (6.7.3).
type CDeclSpec = CDeclarationSpecifier NodeInfo
data CDeclarationSpecifier a
  = CStorageSpec (CStorageSpecifier a) -- ^ storage-class specifier or typedef
  | CTypeSpec    (CTypeSpecifier a)    -- ^ type name
  | CTypeQual    (CTypeQualifier a)    -- ^ type qualifier
  | CFunSpec     (CFunctionSpecifier a) -- ^ function specifier
  | CAlignSpec   (CAlignmentSpecifier a) -- ^ alignment specifier
    deriving (Typeable, Generic, Generic1 {-! ,CNode ,Functor, Annotated !-})


-- | Separate the declaration specifiers
--
-- @__attribute__@ of a declaration qualify declarations or declarators (but not types),
-- and are therefore separated as well.
partitionDeclSpecs :: [CDeclarationSpecifier a]
                   -> ( [CStorageSpecifier a], [CAttribute a]
                      , [CTypeQualifier a], [CTypeSpecifier a]
                      , [CFunctionSpecifier a], [CAlignmentSpecifier a])
partitionDeclSpecs = foldr deals ([],[],[],[],[],[]) where
    deals (CStorageSpec sp) (sts,ats,tqs,tss,fss,ass)  = (sp:sts,ats,tqs,tss,fss,ass)
    deals (CTypeQual (CAttrQual attr)) (sts,ats,tqs,tss,fss,ass)  = (sts,attr:ats,tqs,tss,fss,ass)
    deals (CTypeQual tq) (sts,ats,tqs,tss,fss,ass)     = (sts,ats,tq:tqs,tss,fss,ass)
    deals (CTypeSpec ts) (sts,ats,tqs,tss,fss,ass)     = (sts,ats,tqs,ts:tss,fss,ass)
    deals (CFunSpec fs) (sts,ats,tqs,tss,fss,ass)      = (sts,ats,tqs,tss,fs:fss,ass)
    deals (CAlignSpec as) (sts,ats,tqs,tss,fss,ass)    = (sts,ats,tqs,tss,fss,as:ass)

-- | C storage class specifier (and typedefs) (K&R A8.1, C99 6.7.1)
type CStorageSpec = CStorageSpecifier NodeInfo
data CStorageSpecifier a
  = CAuto     (AnnMisc a)     -- ^ auto
  | CRegister (AnnMisc a)     -- ^ register
  | CStatic   (AnnMisc a)     -- ^ static
  | CExtern   (AnnMisc a)     -- ^ extern
  | CTypedef  (AnnMisc a)     -- ^ typedef
  | CThread   (AnnMisc a)     -- ^ C11/GNUC thread local storage
    deriving (Typeable {-! ,CNode ,Functor ,Annotated !-})



-- | C type specifier (K&R A8.2, C99 6.7.2)
--
-- Type specifiers are either basic types such as @char@ or @int@,
-- @struct@, @union@ or @enum@ specifiers or typedef names.
--
-- As a GNU extension, a @typeof@ expression also is a type specifier.
type CTypeSpec = CTypeSpecifier NodeInfo
data CTypeSpecifier a
  = CVoidType    (AnnMisc a)
  | CCharType    (AnnMisc a)
  | CShortType   (AnnMisc a)
  | CIntType     (AnnMisc a)
  | CLongType    (AnnMisc a)
  | CFloatType   (AnnMisc a)
  | CFloat128Type (AnnMisc a)
  | CDoubleType  (AnnMisc a)
  | CSignedType  (AnnMisc a)
  | CUnsigType   (AnnMisc a)
  | CBoolType    (AnnMisc a)
  | CComplexType (AnnMisc a)
  | CInt128Type  (AnnMisc a)
  | CSUType      (CStructureUnion a) (AnnMisc a)      -- ^ Struct or Union specifier
  | CEnumType    (CEnumeration a)    (AnnMisc a)      -- ^ Enumeration specifier
  | CTypeDef     Ident        (AnnMisc a)      -- ^ Typedef name
  | CTypeOfExpr  (CExpression a)  (AnnMisc a)  -- ^ @typeof(expr)@
  | CTypeOfType  (CDeclaration a) (AnnMisc a)  -- ^ @typeof(type)@
  | CAtomicType  (CDeclaration a) (AnnMisc a)  -- ^ @_Atomic(type)@
    deriving (Typeable {-! ,CNode ,Functor ,Annotated !-})


-- | returns @True@ if the given typespec is a struct, union or enum /definition/
isSUEDef :: CTypeSpecifier a -> Bool
isSUEDef (CSUType (CStruct _ _ (Just _) _ _) _) = True
isSUEDef (CEnumType (CEnum _ (Just _) _ _) _)   = True
isSUEDef _                                      = False

-- | C type qualifiers (K&R A8.2, C99 6.7.3) and attributes.
--
-- @const@, @volatile@ and @restrict@ type qualifiers
-- Additionally, @__attribute__@ annotations for declarations and declarators, and
-- function specifiers
type CTypeQual = CTypeQualifier NodeInfo
data CTypeQualifier a
  = CConstQual (AnnMisc a)
  | CVolatQual (AnnMisc a)
  | CRestrQual (AnnMisc a)
  | CAtomicQual (AnnMisc a)
  | CAttrQual  (CAttribute a)
  | CNullableQual (AnnMisc a)
  | CNonnullQual (AnnMisc a)
    deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})


-- | C function specifiers (C99 6.7.4)
--
-- function specifiers @inline@ and @_Noreturn@
type CFunSpec = CFunctionSpecifier NodeInfo
data CFunctionSpecifier a
  = CInlineQual (AnnMisc a)
  | CNoreturnQual (AnnMisc a)
    deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})


-- | C alignment specifiers (C99 6.7.5)
--
type CAlignSpec = CAlignmentSpecifier NodeInfo
data CAlignmentSpecifier a
  = CAlignAsType (CDeclaration a) (AnnMisc a)  -- ^ @_Alignas(type)@
  | CAlignAsExpr (CExpression a) (AnnMisc a)   -- ^ @_Alignas(expr)@
    deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})


-- | C structure or union specifiers (K&R A8.3, C99 6.7.2.1)
--
-- @CStruct tag identifier struct-decls c-attrs@ represents a struct or union specifier (depending on @tag@).
--
--   * either @identifier@ or the declaration list @struct-decls@ (or both) have to be present.
--
--     Example: in @struct foo x;@, the identifier is present, in @struct { int y; } x@ the declaration list, and
--     in @struct foo { int y; } x;@ both of them.
--
--   * @c-attrs@ is a list of @__attribute__@ annotations associated with the struct or union specifier
type CStructUnion = CStructureUnion NodeInfo
data CStructureUnion a
  = CStruct
    CStructTag
    (Maybe Ident)
    (Maybe [CDeclaration a])  -- member declarations
    [CAttribute a]            -- __attribute__s
    (AnnMisc a)
    deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})


-- | A tag to determine wheter we refer to a @struct@ or @union@, see 'CStructUnion'.
data CStructTag = CStructTag
                | CUnionTag
                deriving (Show, Eq,Data,Typeable, Generic)

instance NFData CStructTag

-- | C enumeration specifier (K&R A8.4, C99 6.7.2.2)
--
-- @CEnum identifier enumerator-list attrs@ represent as enum specifier
--
--  * Either the identifier or the enumerator-list (or both) have to be present.
--
--  * If @enumerator-list@ is present, it has to be non-empty.
--
--  * The enumerator list is of the form @(enumeration-constant, enumeration-value?)@, where the latter
--    is an optional constant integral expression.
--
--  * @attrs@ is a list of @__attribute__@ annotations associated with the enumeration specifier
type CEnum = CEnumeration NodeInfo
data CEnumeration a
  = CEnum
    (Maybe Ident)
    (Maybe [(Ident,                   -- variant name
             Maybe (CExpression a))]) -- explicit variant value
    [CAttribute a]                    -- __attribute__s
    a
    deriving (Typeable, Generic, Generic1 {-! ,CNode ,Functor ,Annotated !-})


-- | C initialization (K&R A8.7, C99 6.7.8)
--
-- Initializers are either assignment expressions or initializer lists
-- (surrounded in curly braces), whose elements are themselves
-- initializers, paired with an optional list of designators.
type CInit = CInitializer NodeInfo
data CInitializer a
  -- | assignment expression
  = CInitExpr (CExpression a) (AnnInitializer a)
  -- | initialization list (see 'CInitList')
  | CInitList (CInitializerList a) (AnnInitializer a)
    deriving (Typeable, Generic {-! ,CNode , Annotated !-})


-- | Initializer List
--
-- The members of an initializer list are of the form @(designator-list,initializer)@.
-- The @designator-list@ specifies one member of the compound type which is initialized.
-- It is allowed to be empty - in this case the initializer refers to the
-- ''next'' member of the compound type (see C99 6.7.8).
--
-- Examples (simplified expressions and identifiers):
--
-- > -- int x[3][4] = { [0][3] = 4, [2] = 5, 8 };
-- > --   corresponds to the assignments
-- > -- x[0][3] = 4; x[2][0] = 5; x[2][1] = 8;
-- > let init1 = ([CArrDesig 0, CArrDesig 3], CInitExpr 4)
-- >     init2 = ([CArrDesig 2]             , CInitExpr 5)
-- >     init3 = ([]                        , CInitExpr 8)
-- > in  CInitList [init1, init2, init3]
--
-- > -- struct { struct { int a[2]; int b[2]; int c[2]; } s; } x = { .s = { {2,3} , .c[0] = 1 } };
-- > --   corresponds to the assignments
-- > -- x.s.a[0] = 2; x.s.a[1] = 3; x.s.c[0] = 1;
-- > let init_s_0 = CInitList [ ([], CInitExpr 2), ([], CInitExpr 3)]
-- >     init_s   = CInitList [
-- >                            ([], init_s_0),
-- >                            ([CMemberDesig "c", CArrDesig 0], CInitExpr 1)
-- >                          ]
-- > in  CInitList [(CMemberDesig "s", init_s)]
type CInitList = CInitializerList NodeInfo
type CInitializerList a = [([CPartDesignator a], CInitializer a)]

-- | Designators
--
-- A designator specifies a member of an object, either an element or range of an array,
-- or the named member of a struct \/ union.
type CDesignator = CPartDesignator NodeInfo
data CPartDesignator a
  -- | array position designator
  = CArrDesig     (CExpression a) a
  -- | member designator
  | CMemberDesig  Ident a
  -- | array range designator @CRangeDesig from to _@ (GNU C)
  | CRangeDesig (CExpression a) (CExpression a) a
    deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})


-- | @__attribute__@ annotations
--
-- Those are of the form @CAttr attribute-name attribute-parameters@,
-- and serve as generic properties of some syntax tree elements.
type CAttr = CAttribute NodeInfo
data CAttribute a = CAttr Ident [CExpression a] (AnnMisc a)
                    deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})


-- | C expression (K&R A7)
--
-- * these can be arbitrary expression, as the argument of `sizeof' can be
--   arbitrary, even if appearing in a constant expression
--
-- * GNU C extensions: @alignof@, @__real@, @__imag@, @({ stmt-expr })@, @&& label@ and built-ins
--
type CExpr = CExpression NodeInfo
data CExpression a
  = CComma       [CExpression a]         -- comma expression list, n >= 2
                 (AnnExpression a)
  | CAssign      CAssignOp               -- assignment operator
                 (CExpression a)         -- l-value
                 (CExpression a)         -- r-value
                 (AnnExpression a)
  | CCond        (CExpression a)         -- conditional
                 (Maybe (CExpression a)) -- true-expression (GNU allows omitting)
                 (CExpression a)         -- false-expression
                 (AnnExpression a)
  | CBinary      CBinaryOp               -- binary operator
                 (CExpression a)         -- lhs
                 (CExpression a)         -- rhs
                 (AnnExpression a)
  | CCast        (CDeclaration a)        -- type name
                 (CExpression a)
                 (AnnExpression a)
  | CUnary       CUnaryOp                -- unary operator
                 (CExpression a)
                 (AnnExpression a)
  | CSizeofExpr  (CExpression a)
                 (AnnExpression a)
  | CSizeofType  (CDeclaration a)        -- type name
                 (AnnExpression a)
  | CAlignofExpr (CExpression a)
                 (AnnExpression a)
  | CAlignofType (CDeclaration a)        -- type name
                 (AnnExpression a)
  | CComplexReal (CExpression a)         -- real part of complex number
                 (AnnExpression a)
  | CComplexImag (CExpression a)         -- imaginary part of complex number
                 (AnnExpression a)
  | CIndex       (CExpression a)         -- array
                 (CExpression a)         -- index
                 (AnnExpression a)
  | CCall
    (CExpression a)         -- function
                 [CExpression a]         -- arguments
                 (AnnExpression a)
  | CMember      (CExpression a)         -- structure
                 Ident                   -- member name
                 Bool                    -- deref structure? (True for `->')
                 (AnnExpression a)
  | CVar         Ident                   -- identifier (incl. enumeration const)
                 (AnnExpression a)
  | CConst       (CConstant a)           -- ^ integer, character, floating point and string constants
  | CCompoundLit (CDeclaration a)
                 (CInitializerList a)    -- type name & initialiser list
                 (AnnExpression a)
  | CGenericSelection (CExpression a) [(Maybe (CDeclaration a), CExpression a)] (AnnExpression a) -- ^ C11 generic selection
  | CStatExpr    (CStatement a) (AnnExpression a)        -- ^ GNU C compound statement as expr
  | CLabAddrExpr Ident (AnnExpression a)                 -- ^ GNU C address of label
  | CBuiltinExpr (CBuiltinThing a)       -- ^ builtin expressions, see 'CBuiltin'
    deriving (Typeable,Generic {-! ,CNode , Annotated !-})



-- | GNU Builtins, which cannot be typed in C99
type CBuiltin = CBuiltinThing NodeInfo
data CBuiltinThing a
  = CBuiltinVaArg (CExpression a) (CDeclaration a) a            -- ^ @(expr, type)@
  | CBuiltinOffsetOf (CDeclaration a) [CPartDesignator a] a -- ^ @(type, designator-list)@
  | CBuiltinTypesCompatible (CDeclaration a) (CDeclaration a) a  -- ^ @(type,type)@
  | CBuiltinConvertVector (CExpression a) (CDeclaration a) a -- ^ @(expr, type)@
    deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})


-- | C constant (K&R A2.5 & A7.2)
type CConst = CConstant NodeInfo
data CConstant a
  = CIntConst   CInteger (AnnConstant a)
  | CCharConst  CChar (AnnConstant a)
  | CFloatConst CFloat (AnnConstant a)
  | CStrConst   CString (AnnConstant a)
    deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})


-- | Attributed string literals
type CStrLit = CStringLiteral NodeInfo
data CStringLiteral a = CStrLit CString (AnnStringLiteral a)
            deriving (Typeable, Generic {-! ,CNode ,Functor ,Annotated !-})


cstringOfLit :: CStringLiteral a -> CString
cstringOfLit (CStrLit cstr _) = cstr

-- | Lift a string literal to a C constant
liftStrLit :: CStringLiteral NodeInfo -> CConstant NodeInfo
liftStrLit (CStrLit str at) = CStrConst str at


-- fmap2 :: (a->a') -> (a,b) -> (a',b)
-- fmap2 f (a,b) = (f a, b)

-- Instances generated using derive-2.* (not anymore :-())
-- GENERATED START

instance CNode (CTranslationUnit NodeInfo) where
        nodeInfo (CTranslUnit _ n) = n

instance Pos (CTranslationUnit NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode (CExternalDeclaration NodeInfo) where
        nodeInfo (CDeclExt d)  = nodeInfo d
        nodeInfo (CFDefExt d)  = nodeInfo d
        nodeInfo (CAsmExt _ n) = nodeInfo n

instance Pos (CExternalDeclaration NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode (CFunctionDef NodeInfo) where
        nodeInfo (CFunDef _ _ _ _ n) = n

instance Pos (CFunctionDef NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode (CDeclaration NodeInfo) where
        nodeInfo (CDecl _ _ n)         = nodeInfo n
        nodeInfo (CStaticAssert _ _ n) = nodeInfo n

instance Pos (CDeclaration NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode (CDeclarator NodeInfo) where
        nodeInfo (CDeclr _ _ _ _ n) = nodeInfo n
instance Pos (CDeclarator NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode (CDerivedDeclarator NodeInfo) where
        nodeInfo (CPtrDeclr _ n)   = nodeInfo n
        nodeInfo (CArrDeclr _ _ n) = nodeInfo n
        nodeInfo (CFunDeclr _ _ n) = nodeInfo n
instance Pos (CDerivedDeclarator NodeInfo) where
        posOf x = posOf (nodeInfo x)

instance CNode (CStatement NodeInfo) where
        nodeInfo (CLabel _ _ _ n)  = nodeInfo n
        nodeInfo (CCase _ _ n)     = nodeInfo n
        nodeInfo (CCases _ _ _ n)  = nodeInfo n
        nodeInfo (CDefault _ n)    = nodeInfo n
        nodeInfo (CExpr _ n)       = nodeInfo n
        nodeInfo (CCompound _ _ n) = nodeInfo n
        nodeInfo (CIf _ _ _ n)     = nodeInfo n
        nodeInfo (CSwitch _ _ n)   = nodeInfo n
        nodeInfo (CWhile _ _ _ n)  = nodeInfo n
        nodeInfo (CFor _ _ _ _ n)  = nodeInfo n
        nodeInfo (CGoto _ n)       = nodeInfo n
        nodeInfo (CGotoPtr _ n)    = nodeInfo n
        nodeInfo (CCont d)         = nodeInfo d
        nodeInfo (CBreak d)        = nodeInfo d
        nodeInfo (CReturn _ n)     = nodeInfo n
        nodeInfo (CAsm _ n)        = nodeInfo n
instance Pos (CStatement NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode t1 => CNode (CAssemblyStatement t1) where
        nodeInfo (CAsmStmt _ _ _ _ _ n) = nodeInfo n
instance CNode t1 => Pos (CAssemblyStatement t1) where
        posOf x = posOf (nodeInfo x)


instance CNode t1 => CNode (CAssemblyOperand t1) where
        nodeInfo (CAsmOperand _ _ _ n) = nodeInfo n
instance CNode t1 => Pos (CAssemblyOperand t1) where
        posOf x = posOf (nodeInfo x)


instance CNode (CCompoundBlockItem NodeInfo) where
        nodeInfo (CBlockStmt d)    = nodeInfo d
        nodeInfo (CBlockDecl d)    = nodeInfo d
        nodeInfo (CNestedFunDef d) = nodeInfo d
instance Pos (CCompoundBlockItem NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode (CDeclarationSpecifier NodeInfo) where
        nodeInfo (CStorageSpec d) = nodeInfo d
        nodeInfo (CTypeSpec d)    = nodeInfo d
        nodeInfo (CTypeQual d)    = nodeInfo d
        nodeInfo (CFunSpec d)     = nodeInfo d
        nodeInfo (CAlignSpec d)   = nodeInfo d

instance Pos (CDeclarationSpecifier NodeInfo) where
        posOf x = posOf (nodeInfo x)

instance CNode (CStorageSpecifier NodeInfo) where
        nodeInfo (CAuto d)     = nodeInfo d
        nodeInfo (CRegister d) = nodeInfo d
        nodeInfo (CStatic d)   = nodeInfo d
        nodeInfo (CExtern d)   = nodeInfo d
        nodeInfo (CTypedef d)  = nodeInfo d
        nodeInfo (CThread d)   = nodeInfo d

instance Pos (CStorageSpecifier NodeInfo) where
        posOf x = posOf (nodeInfo x)

instance CNode (CTypeSpecifier NodeInfo) where
        nodeInfo (CVoidType d)     = nodeInfo d
        nodeInfo (CCharType d)     = nodeInfo d
        nodeInfo (CShortType d)    = nodeInfo d
        nodeInfo (CIntType d)      = nodeInfo d
        nodeInfo (CLongType d)     = nodeInfo d
        nodeInfo (CFloatType d)    = nodeInfo d
        nodeInfo (CFloat128Type d) = nodeInfo d
        nodeInfo (CDoubleType d)   = nodeInfo d
        nodeInfo (CSignedType d)   = nodeInfo d
        nodeInfo (CUnsigType d)    = nodeInfo d
        nodeInfo (CBoolType d)     = nodeInfo d
        nodeInfo (CComplexType d)  = nodeInfo d
        nodeInfo (CInt128Type d)   = nodeInfo d
        nodeInfo (CSUType _ n)     = nodeInfo n
        nodeInfo (CEnumType _ n)   = nodeInfo n
        nodeInfo (CTypeDef _ n)    = nodeInfo n
        nodeInfo (CTypeOfExpr _ n) = nodeInfo n
        nodeInfo (CTypeOfType _ n) = nodeInfo n
        nodeInfo (CAtomicType _ n) = nodeInfo n
instance Pos (CTypeSpecifier NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode (CTypeQualifier NodeInfo) where
        nodeInfo (CConstQual d)    = nodeInfo d
        nodeInfo (CVolatQual d)    = nodeInfo d
        nodeInfo (CRestrQual d)    = nodeInfo d
        nodeInfo (CAtomicQual d)   = nodeInfo d
        nodeInfo (CAttrQual d)     = nodeInfo d
        nodeInfo (CNullableQual d) = nodeInfo d
        nodeInfo (CNonnullQual d)  = nodeInfo d

instance Pos (CTypeQualifier NodeInfo) where
        posOf x = posOf (nodeInfo x)



instance CNode (CFunctionSpecifier NodeInfo) where
        nodeInfo (CInlineQual d)   = nodeInfo d
        nodeInfo (CNoreturnQual d) = nodeInfo d

instance Pos (CFunctionSpecifier NodeInfo) where
        posOf x = posOf (nodeInfo x)

instance CNode (CAlignmentSpecifier NodeInfo) where
        nodeInfo (CAlignAsType _ n) = nodeInfo n
        nodeInfo (CAlignAsExpr _ n) = nodeInfo n

instance Pos (CAlignmentSpecifier NodeInfo) where
        posOf x = posOf (nodeInfo x)

instance CNode (CStructureUnion NodeInfo) where
        nodeInfo (CStruct _ _ _ _ n) = nodeInfo n

instance Pos (CStructureUnion NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode t1 => CNode (CEnumeration t1) where
        nodeInfo (CEnum _ _ _ n) = nodeInfo n
instance CNode t1 => Pos (CEnumeration t1) where
        posOf x = posOf (nodeInfo x)


instance CNode (CInitializer NodeInfo) where
        nodeInfo (CInitExpr _ n) = nodeInfo n
        nodeInfo (CInitList _ n) = nodeInfo n

instance Pos (CInitializer NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode t1 => CNode (CPartDesignator t1) where
        nodeInfo (CArrDesig _ n)     = nodeInfo n
        nodeInfo (CMemberDesig _ n)  = nodeInfo n
        nodeInfo (CRangeDesig _ _ n) = nodeInfo n
instance CNode t1 => Pos (CPartDesignator t1) where
        posOf x = posOf (nodeInfo x)


instance CNode (CAttribute NodeInfo) where
        nodeInfo (CAttr _ _ n) = nodeInfo n

instance Pos (CAttribute NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode (CExpression NodeInfo) where
        nodeInfo (CComma _ n)              = n
        nodeInfo (CAssign _ _ _ n)         = n
        nodeInfo (CCond _ _ _ n)           = n
        nodeInfo (CBinary _ _ _ n)         = n
        nodeInfo (CCast _ _ n)             = n
        nodeInfo (CUnary _ _ n)            = n
        nodeInfo (CSizeofExpr _ n)         = n
        nodeInfo (CSizeofType _ n)         = n
        nodeInfo (CAlignofExpr _ n)        = n
        nodeInfo (CAlignofType _ n)        = n
        nodeInfo (CComplexReal _ n)        = n
        nodeInfo (CComplexImag _ n)        = n
        nodeInfo (CIndex _ _ n)            = n
        nodeInfo (CCall _ _ n)             = n
        nodeInfo (CMember _ _ _ n)         = n
        nodeInfo (CVar _ n)                = n
        nodeInfo (CConst d)                = nodeInfo d
        nodeInfo (CCompoundLit _ _ n)      = n
        nodeInfo (CGenericSelection _ _ n) = n
        nodeInfo (CStatExpr _ n)           = n
        nodeInfo (CLabAddrExpr _ n)        = n
        nodeInfo (CBuiltinExpr d)          = nodeInfo d

instance Pos (CExpression NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode t1 => CNode (CBuiltinThing t1) where
        nodeInfo (CBuiltinVaArg _ _ n)           = nodeInfo n
        nodeInfo (CBuiltinOffsetOf _ _ n)        = nodeInfo n
        nodeInfo (CBuiltinTypesCompatible _ _ n) = nodeInfo n
        nodeInfo (CBuiltinConvertVector _ _ n)   = nodeInfo n
instance CNode t1 => Pos (CBuiltinThing t1) where
        posOf x = posOf (nodeInfo x)


instance CNode (CConstant NodeInfo) where
        nodeInfo (CIntConst _ n)   = nodeInfo n
        nodeInfo (CCharConst _ n)  = nodeInfo n
        nodeInfo (CFloatConst _ n) = nodeInfo n
        nodeInfo (CStrConst _ n)   = nodeInfo n

instance Pos (CConstant NodeInfo) where
        posOf x = posOf (nodeInfo x)


instance CNode (CStringLiteral NodeInfo) where
        nodeInfo (CStrLit _ n) = nodeInfo n
instance Pos (CStringLiteral NodeInfo) where
        posOf x = posOf (nodeInfo x)

-- GENERATED STOP
