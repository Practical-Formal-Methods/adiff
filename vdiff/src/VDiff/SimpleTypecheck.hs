{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
-- I don't care about GNU extensions, so statements are always of type void

module VDiff.SimpleTypecheck
  ( simpleTypechecker
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Coerce
import qualified Data.Map                   as Map
import           Language.C.Analysis.SemRep hiding (constant)
import qualified Prelude                    as P
import           VDiff.Prelude              hiding (declarator, expression,
                                             externalDeclaration,
                                             functionDefinition,
                                             translationUnit)

return' a = Right (a, [])


data Scope
  = Scope
  { variables :: Map Ident Type
  , typedefs  :: Map Ident Type
  } deriving (Show)

addVarToScope :: Ident -> Type -> Scope -> Scope
addVarToScope n ty s = s { variables = Map.insert n ty (variables s)}
addTypedefToScope :: Ident -> Type -> Scope -> Scope
addTypedefToScope n decls s = s { typedefs = Map.insert n decls (typedefs s) }


data AnalysisState = AnalysisState
  { scope      :: Scope
  , structures :: Map.Map Ident (Map.Map Ident Type)
  } deriving (Show)

newtype AnalysisM a
  = AnalysisM
  { unAnalysisM :: StateT Scope (Except [CError]) a
  } deriving (Functor, Applicative, Monad)

getScope :: AnalysisM Scope
getScope = AnalysisM get

putScope :: Scope -> AnalysisM ()
putScope s = AnalysisM (put s)

defaultScope :: Scope
defaultScope = Scope Map.empty Map.empty

declare :: Ident -> Type -> AnalysisM ()
declare n ty = do
  traceM $ "declaring " <> tshow (identToString n) <> " :: " <> tshow ty
  s <- getScope
  let s' = addVarToScope n ty s
  putScope s'

-- This one will never throw an error. If it is not found it returns the "int" as type
lookupVar :: Ident -> AnalysisM Type
lookupVar n = do
  s <- getScope
  case Map.lookup n (variables s) of
    Nothing -> trace ("failed lookup for " <> tshow (identToString n)) $ return simpleInt
    Just ty -> return ty

lookupTypedef :: Ident -> AnalysisM Type
lookupTypedef  n = do
  s <- getScope
  case Map.lookup n (typedefs s) of
    Nothing -> trace ("failed lookup for " <> tshow (identToString n)) $ return simpleInt
    Just ty -> return ty

-- enters a new scope, names declared inside will not be declared outside
enterScope :: AnalysisM a -> AnalysisM a
enterScope action = do
  s <- getScope
  x <- action
  putScope s
  return x

simpleTypechecker :: Typechecker
simpleTypechecker tu = case runExcept (runStateT (unAnalysisM (translationUnit tu)) defaultScope) of
  Left errs   -> Left errs
  Right (a,_) -> Right (a,[])

-- | annotate every expression with type "int"
translationUnit :: CTranslationUnit NodeInfo -> AnalysisM (CTranslationUnit SemPhase)
translationUnit (CTranslUnit declrs ni) = CTranslUnit <$> mapM externalDeclaration declrs <*> pure ni


externalDeclaration :: CExternalDeclaration NodeInfo -> AnalysisM (CExternalDeclaration SemPhase)
externalDeclaration (CDeclExt declr) = CDeclExt <$> declaration declr
externalDeclaration (CFDefExt fdef ) = CFDefExt <$> functionDefinition fdef
externalDeclaration (CAsmExt blk ni) = fail "ASM blocks not implemented"

functionDefinition :: CFunctionDef NodeInfo -> AnalysisM (CFunctionDef SemPhase)
functionDefinition (CFunDef specs declr oldStyleParams body ni) = do
  specs' <- mapM declarationSpecifier specs
  (f, n,ty) <- enterScope $ do
    declr' <- declarator declr
    Just (n,ty) <- calculateTypeOfDeclarator specs' declr'
    oldStyleParams' <- mapM declaration oldStyleParams
    body' <- statement body
    return (CFunDef specs' declr' oldStyleParams' body' ni, n, ty)
  declare n ty
  return f

declaration :: CDeclaration NodeInfo -> AnalysisM (CDeclaration SemPhase)
declaration (CStaticAssert expr lit ni) = fail "static asserts not implemented"
declaration (CDecl specs initDeclrs ni) = do
  specs' <- mapM declarationSpecifier specs
  initDeclrs' <- mapM initializedDeclarator initDeclrs
  forM_ initDeclrs' $ \(mdeclr,_,_) ->
    case mdeclr of
      Just declr -> do
        calculateTypeOfDeclarator specs' declr >>= \case
          Just (n,ty) -> declare n ty
          Nothing     -> return ()
      _ -> return ()

  return $ CDecl specs' initDeclrs' ni

calculateTypeOfDeclarator :: [CDeclarationSpecifier SemPhase] -> CDeclarator SemPhase -> AnalysisM (Maybe (Ident, Type))
calculateTypeOfDeclarator specs (CDeclr (Just n) derived _ _ _) = do
        start <- calculateTypeFromSpecs (getTypeSpecs specs)
        let ty = foldF (map analyseDerivedDeclarator derived)  start
        return $ Just (n, ty)
calculateTypeOfDeclarator specs (CDeclr Nothing derived _ _ _) = return Nothing


-- apply a bunch a functions sequentially.
foldF :: [a -> a] -> a -> a
foldF [] x     = x
foldF (f:fs) x = f (foldF fs x)


getTypeSpecs :: [CDeclarationSpecifier a] -> [CTypeSpecifier a]
getTypeSpecs = (\(_,_,_,x,_,_) -> x) . partitionDeclSpecs

calculateTypeFromSpecs :: [CTypeSpecifier SemPhase] -> AnalysisM Type
calculateTypeFromSpecs specs = do
  specs' <- mapM analyseTypeSpecifier specs
  return $ f specs'
  where
    f []              = trace "defaulting to int" $ simpleInt
    f [Left ty]       = ty
    f (Right fty : l) = fty (f l)
    f _               = trace "oops" $ simpleInt


-- turns a 'DerivedDeclarator' into a function that transforms the type. E.g.
-- Given a @CPtrDeclr []@, this turns into a function that turns a type T into
-- the type *T.
-- TODO: I'm ignoring TypeQuals here
analyseDerivedDeclarator :: CDerivedDeclarator SemPhase -> Type -> Type
analyseDerivedDeclarator (CPtrDeclr quals _) t = PtrType t noTypeQuals []
analyseDerivedDeclarator (CFunDeclr x attrs  ni)  t =
  case x of
    Left ns          -> FunctionType (FunType t [] False) noAttributes
    Right (params,b) -> FunctionType (FunType t [] b) noAttributes

analyseDerivedDeclarator (CArrDeclr _ _ _ )  t = ArrayType t (UnknownArraySize False) noTypeQuals noAttributes -- TODO

analyseTypeSpecifier :: CTypeSpecifier SemPhase -> AnalysisM (Either Type (Type -> Type))
analyseTypeSpecifier (CVoidType _)             = return $ Left $ DirectType  TyVoid noTypeQuals noAttributes
analyseTypeSpecifier (CCharType _)             = return $ Left $ DirectType (TyIntegral TyChar) noTypeQuals noAttributes
analyseTypeSpecifier (CIntType _)              = return $ Left $ DirectType (TyIntegral TyInt) noTypeQuals noAttributes
analyseTypeSpecifier (CShortType _)            = return $ Left $ DirectType (TyIntegral TyShort)  noTypeQuals noAttributes
analyseTypeSpecifier (CLongType _)             = return $ Left $ DirectType (TyIntegral TyLong)  noTypeQuals noAttributes
analyseTypeSpecifier (CFloatType _)            = return $ Left $ DirectType (TyFloating TyFloat)  noTypeQuals noAttributes
analyseTypeSpecifier (CFloat128Type _)         = return $ Left $ DirectType (TyFloating TyFloat128)  noTypeQuals noAttributes
analyseTypeSpecifier (CDoubleType _ )          = return $ Left $ DirectType (TyFloating TyDouble)  noTypeQuals noAttributes
analyseTypeSpecifier (CSignedType _ )          = return $ Right id
analyseTypeSpecifier (CUnsigType _ )           = return $ Right unsignType
analyseTypeSpecifier (CBoolType _ )            = return $ Left $ DirectType (TyIntegral TyBool) noTypeQuals noAttributes
analyseTypeSpecifier (CComplexType  _)         = return $ undefined
analyseTypeSpecifier (CInt128Type _)           = return $ undefined

analyseTypeSpecifier (CSUType sue  _) = do
  let (CStruct tag mi mdecls attrs ni) = sue
  let sueRef = case mi of
        Nothing -> NamedRef (internalIdent "someEnum")
        Just i  -> NamedRef i
  let compType = case tag of
        CStructTag -> StructTag
        CUnionTag  -> UnionTag
  return $ Left $ DirectType (TyComp $ CompTypeRef sueRef compType ni) noTypeQuals noAttributes

analyseTypeSpecifier (CEnumType enum ni)       = do
  let (CEnum mi ml attrs enumNi) = enum
  let sueRef = case mi of
        Nothing -> NamedRef (internalIdent "someEnum")
        Just i  -> NamedRef i
  return $ Left $ DirectType  (TyEnum (EnumTypeRef sueRef ni) ) noTypeQuals noAttributes
analyseTypeSpecifier (CTypeDef n _)            = do
  ty <- lookupTypedef n
  return $ Left ty
analyseTypeSpecifier (CTypeOfExpr expr _)      = return $ Left $ getType expr
analyseTypeSpecifier (CTypeOfType decl _)      = return $ undefined
analyseTypeSpecifier (CAtomicType decl _)      = return $ undefined




initializedDeclarator :: (Maybe (CDeclarator NodeInfo), Maybe (CInitializer NodeInfo), Maybe (CExpression NodeInfo))
  -> AnalysisM (Maybe (CDeclarator SemPhase), Maybe (CInitializer SemPhase), Maybe (CExpression SemPhase))
initializedDeclarator (md, mi, me) = do
  md' <- maybeM declarator md
  mi' <- maybeM initializer mi
  me' <- maybeM expression me
  return (md', mi', me')

initializer :: CInitializer NodeInfo -> AnalysisM (CInitializer SemPhase)
initializer (CInitExpr e ni) = CInitExpr <$> expression e <*> pure ni
initializer (CInitList iniList ni) = CInitList <$> mapM initPart iniList <*> pure ni
  where
    initPart :: ([CPartDesignator NodeInfo], CInitializer NodeInfo) -> AnalysisM ([CPartDesignator SemPhase], CInitializer SemPhase)
    initPart (prts, init) = do
      prts' <- mapM partDesignator prts
      init' <- initializer init
      return (prts', init')
    partDesignator (CArrDesig e ni) = CArrDesig <$> expression e <*> pure ni
    partDesignator (CMemberDesig n ni) = return $ CMemberDesig n ni
    partDesignator (CRangeDesig e1 e2 ni) = CRangeDesig <$> expression e1 <*> expression e2 <*> pure ni


declarationSpecifier :: CDeclarationSpecifier NodeInfo -> AnalysisM (CDeclarationSpecifier SemPhase)
declarationSpecifier (CStorageSpec sspec) = CStorageSpec <$> storageSpecifier sspec
declarationSpecifier (CTypeSpec tspec)    = CTypeSpec <$> typeSpecifier tspec
declarationSpecifier (CFunSpec fspec)     = CFunSpec <$> functionSpecifier fspec
declarationSpecifier (CTypeQual tqual)    = CTypeQual <$> typeQualifier tqual
declarationSpecifier (CAlignSpec aspec)   = CAlignSpec <$> alignmentSpecifier aspec

functionSpecifier :: CFunctionSpecifier NodeInfo -> AnalysisM (CFunctionSpecifier SemPhase)
functionSpecifier (CInlineQual ni)   = return $ CInlineQual ni
functionSpecifier (CNoreturnQual ni) = return $ CNoreturnQual ni

storageSpecifier :: CStorageSpecifier NodeInfo -> AnalysisM (CStorageSpecifier SemPhase)
storageSpecifier (CAuto ni)     = return $ CAuto ni
storageSpecifier (CRegister ni) = return $ CRegister ni
storageSpecifier (CStatic ni)   = return $ CStatic ni
storageSpecifier (CExtern ni)   = return $ CExtern ni
storageSpecifier (CTypedef ni)  = return $ CTypedef ni
storageSpecifier (CThread ni)   = return $ CThread ni

alignmentSpecifier :: CAlignmentSpecifier NodeInfo -> AnalysisM (CAlignmentSpecifier SemPhase)
alignmentSpecifier (CAlignAsType decl ni) = CAlignAsType <$> declaration decl <*> pure ni
alignmentSpecifier (CAlignAsExpr e ni)    = CAlignAsExpr <$> expression e <*> pure ni

typeSpecifier :: CTypeSpecifier NodeInfo -> AnalysisM (CTypeSpecifier SemPhase)
typeSpecifier (CVoidType  ni)       = return $ CVoidType ni
typeSpecifier (CCharType ni)        = return $ CCharType ni
typeSpecifier (CShortType ni)       = return $ CShortType ni
typeSpecifier (CIntType ni)         = return $ CIntType ni
typeSpecifier (CLongType ni)        = return $ CLongType ni
typeSpecifier (CFloatType ni)       = return $ CFloatType ni
typeSpecifier (CFloat128Type ni)    = return $ CFloat128Type ni
typeSpecifier (CDoubleType ni)      = return $ CDoubleType ni
typeSpecifier (CSignedType ni)      = return $ CSignedType ni
typeSpecifier (CUnsigType ni)       = return $ CUnsigType ni
typeSpecifier (CBoolType ni)        = return $ CBoolType ni
typeSpecifier (CComplexType ni)     = return $ CComplexType ni
typeSpecifier (CInt128Type ni)      = return $ CInt128Type ni
typeSpecifier (CSUType strun  ni)   = CSUType <$> structOrUnion strun <*> pure ni
typeSpecifier (CEnumType enum ni)   = CEnumType <$> enumeration enum <*> pure ni
typeSpecifier (CTypeDef n  ni)      = return $ CTypeDef n ni
typeSpecifier (CTypeOfExpr e ni)    = CTypeOfExpr <$> expression e <*> pure ni
typeSpecifier (CTypeOfType decl ni) = CTypeOfType <$> declaration decl <*> pure ni
typeSpecifier (CAtomicType decl ni) = CAtomicType <$> declaration decl <*> pure ni


enumeration (CEnum mi elems attrs ni) = CEnum mi <$> maybeM enumElements elems <*> mapM attribute attrs <*> pure ni
  where
    enumElements = mapM enumElement
    enumElement (n, Nothing) = return (n, Nothing)
    enumElement (n, Just e) = do
      e' <- expression e
      return (n, Just e')

structOrUnion :: CStructureUnion NodeInfo -> AnalysisM (CStructureUnion SemPhase)
structOrUnion (CStruct tag mi mdecls attrs ni) = do
  mdecls' <- maybeM (mapM declaration) mdecls
  attrs' <- mapM attribute attrs
  return $ CStruct tag mi mdecls' attrs' ni

declarator :: CDeclarator NodeInfo -> AnalysisM (CDeclarator SemPhase)
declarator (CDeclr mi derivedDeclrs mAsm attrs ni) = do
  derivedDeclrs' <- mapM derivedDeclarator derivedDeclrs
  attrs' <- mapM attribute attrs
  mAsm' <- maybeM stringLiteral mAsm
  return $ CDeclr mi derivedDeclrs' Nothing attrs' ni


stringLiteral :: CStringLiteral NodeInfo -> AnalysisM (CStringLiteral SemPhase)
stringLiteral (CStrLit s ni) = return $ CStrLit s ni

derivedDeclarator :: CDerivedDeclarator NodeInfo -> AnalysisM (CDerivedDeclarator SemPhase)
derivedDeclarator (CPtrDeclr typequals ni) = CPtrDeclr <$> mapM typeQualifier typequals <*> pure ni
derivedDeclarator (CArrDeclr typequals arrsize ni) = CArrDeclr <$> mapM typeQualifier typequals <*> arraySize arrsize <*> pure ni
derivedDeclarator (CFunDeclr (Left ns) attrs ni) = do
  attrs' <- mapM attribute attrs
  return $ CFunDeclr (Left ns) attrs' ni

derivedDeclarator (CFunDeclr (Right (decls,b)) attrs ni) = do
  decls' <- mapM declaration decls
  attrs' <- mapM attribute attrs
  return $ CFunDeclr (Right (decls', b)) attrs' ni

typeQualifier :: CTypeQualifier NodeInfo -> AnalysisM (CTypeQualifier SemPhase)
typeQualifier(CConstQual ni)    = return $ CConstQual ni
typeQualifier(CVolatQual ni)    = return $ CVolatQual ni
typeQualifier(CRestrQual ni)    = return $ CRestrQual ni
typeQualifier(CAtomicQual ni)   = return $ CAtomicQual ni
typeQualifier(CAttrQual attr)   = CAttrQual <$> attribute attr
typeQualifier(CNullableQual ni) = return $ CNullableQual ni
typeQualifier(CNonnullQual ni)  = return $ CNonnullQual ni

arraySize :: CArraySize NodeInfo -> AnalysisM (CArraySize SemPhase)
arraySize (CNoArrSize b) = return $ CNoArrSize b
arraySize (CArrSize b e) = CArrSize b <$> expression e

attribute :: CAttribute NodeInfo -> AnalysisM (CAttribute SemPhase)
attribute (CAttr i exprs ni) = CAttr i <$> mapM expression exprs <*> pure ni

statement :: CStatement NodeInfo -> AnalysisM (CStatement SemPhase)
statement (CLabel lbl stmt attrs  ni) = do
  stmt' <- statement stmt
  attrs' <- mapM attribute attrs
  return $ CLabel lbl stmt' attrs' (ni, getType stmt')

statement (CCase e stmt ni) = CCase <$> expression e <*> statement stmt <*> pure (ni, voidType)
statement (CCases e1 e2 stmt ni) = CCases <$> expression e1 <*> expression e2 <*> statement stmt <*> pure (ni, voidType)
statement (CDefault stmt ni) = CDefault <$> statement stmt <*> pure (ni, voidType)
statement (CExpr me ni) =
  case me of
    Nothing -> return $ CExpr Nothing (ni, voidType)
    Just e -> do
      e' <- expression e
      return $ CExpr (Just e') (ni, getType e')

statement (CCompound ns blkItems ni)       = do
  blkItems' <- forM blkItems $ \case
    CBlockStmt stmt -> CBlockStmt <$> statement stmt
    CBlockDecl decl -> CBlockDecl <$> declaration decl
    CNestedFunDef fdef -> CNestedFunDef <$> functionDefinition fdef
  return $ CCompound ns blkItems' (ni, voidType)

statement (CIf e thenStmt mElseStmt ni)    = do
  e' <- expression e
  thenStmt' <- statement thenStmt
  mElseStmt' <- maybeM statement mElseStmt
  return $ CIf e' thenStmt' mElseStmt' (ni, voidType)


statement (CSwitch e stmt ni)               = do
  e' <- expression e
  stmt' <- statement stmt
  return $ CSwitch e' stmt' (ni, voidType)

statement (CWhile e stmt b ni)             = do
  e' <- expression e
  stmt' <- statement stmt
  return $ CWhile e' stmt' b (ni, voidType)

statement (CFor initOrDecl me1 me2 stmt ni) = do
  initOrDecl' <- case initOrDecl of
                   Left me    -> Left <$> maybeM expression me
                   Right decl -> Right <$> declaration decl
  me1' <- maybeM expression me1
  me2' <- maybeM expression me2
  stmt' <- statement stmt
  return $ CFor initOrDecl' me1' me2' stmt' (ni, voidType)

statement (CGoto n ni)                     = return $ CGoto n (ni, voidType)
statement (CGotoPtr e ni)                  = CGotoPtr <$> expression e <*> pure (ni, voidType)
statement (CCont ni)                       = return $ CCont (ni, voidType)
statement (CBreak ni)                      = return $ CBreak (ni, voidType)
statement (CReturn me ni)                  = do
  me' <- case me of
    Nothing -> return Nothing
    Just e  -> Just <$> expression e
  return $ CReturn me' (ni, voidType)
statement (CAsm asmStmt ni)                = undefined


expression :: CExpression NodeInfo -> AnalysisM (CExpression SemPhase)
expression (CComma exprs ni) = do
  exprs' <- mapM expression exprs
  let ty = getType (P.head exprs')
  return $ CComma exprs' (ni, ty)

expression (CAssign op e1 e2 ni) = do
  e1' <- expression e1
  e2' <- expression e2
  return $ CAssign op e1' e2' (ni, getType e1')

expression (CCond e1 me2 e3 ni) = do
  e1' <- expression e1
  me2' <- case me2 of
    Just e2 -> Just <$> expression e2
    Nothing -> return Nothing
  e3' <- expression e3
  return $ CCond e1' me2' e3' (ni, getType e1')

expression (CBinary op e1 e2 ni) = do
  e1' <- expression e1
  e2' <- expression e2
  ty <- if
    | isBooleanOp op -> return simpleInt -- TODO: Make this a 'int refined to bool'
    | otherwise -> unifyArithmetic [getType e1', getType e2']
  return $ CBinary op e1' e2' (ni, ty)

expression (CCast decl e ni) = CCast <$> declaration decl <*> expression e <*> pure (ni, voidType) -- TODO: wrong type
expression (CUnary op e ni) = do
  e' <- expression e
  return $ CUnary op e' (ni, getType e') -- TODO: Check for casting from boolean to int?

expression (CSizeofExpr e ni) = CSizeofExpr <$> expression e <*> pure (ni, simpleInt)

expression (CSizeofType decl ni) = CSizeofType <$> declaration decl <*> pure (ni, simpleInt)

expression (CAlignofExpr e ni) = CAlignofExpr <$> expression e <*> pure (ni, simpleInt)

expression (CAlignofType decl ni) = CAlignofType <$> declaration decl <*> pure (ni, simpleInt)

expression (CComplexReal e ni) = CComplexReal <$> expression e <*> pure (ni, voidType) -- TODO

expression (CComplexImag e ni) = CComplexImag <$> expression e <*> pure (ni, voidType) -- TODO

expression (CIndex e1 e2 ni) = do
  e1' <- expression e1
  e2' <- expression e2
  ty <- applyIndexType (getType e1') (getType e2')
  return $ CIndex e1' e2' (ni, ty)

expression (CCall f ps ni) = do
  f' <- expression f
  ps' <- mapM expression ps
  ty <- applyType (getType f')
  return $ CCall f' ps' (ni, ty)

expression (CMember e n b ni) = do
  e' <- expression e
  return $ CMember e' n b (ni, simpleInt) -- TODO

expression (CVar v ni) = do
  ty <- lookupVar v
  return $ CVar v (ni, ty)

expression (CConst const) = CConst <$> constant const -- integer, character, floating point and string constants

expression (CCompoundLit decl initList ni) = undefined  -- a C99 compound literal
expression (CGenericSelection e ml ni) = undefined  -- a C11 generic selection
expression (CStatExpr stmt ni) = do
  stmt' <- statement stmt
  return $ CStatExpr stmt' (ni, getType stmt')
expression (CLabAddrExpr n ni) = undefined  -- a GNU C address of label
expression (CBuiltinExpr builtin) = CBuiltinExpr <$> builtinThing builtin

constant :: CConstant NodeInfo -> AnalysisM (CConstant SemPhase)
constant (CIntConst c ni)   = return $ CIntConst c (ni, simpleInt)
constant (CCharConst c ni)  = return $ CCharConst c (ni, charType)
constant (CFloatConst c ni) = return $ CFloatConst c (ni, floatType)
constant (CStrConst c ni)   = return $ CStrConst c (ni, stringType)


builtinThing :: CBuiltinThing NodeInfo -> AnalysisM (CBuiltinThing SemPhase)
builtinThing (CBuiltinVaArg e decl ni)                 = do
  e' <- expression e
  decl' <- declaration decl
  return $ CBuiltinVaArg e' decl' ni -- TODO: those things should have a type probably
builtinThing (CBuiltinOffsetOf decl prts ni)           = undefined
builtinThing (CBuiltinTypesCompatible decl1  decl2 ni) = undefined
builtinThing (CBuiltinConvertVector e decl ni)         = undefined

-- unifies the given types
unify :: [Type] -> AnalysisM Type
unify types = return $ P.head types -- TODO: Do some unification

-- unifies the given types
unifyArithmetic :: [Type] -> AnalysisM Type
unifyArithmetic = return . P.foldl1 unifyArithmetic' -- TODO: Do some unification, but make sure that it is numeric
  where
    unifyArithmetic' t u =
      let t' = getIntType t
          u' = getIntType u
      in if
        | integralPrecision t' < integralPrecision u' -> u
        | integralPrecision t' > integralPrecision u' -> t
        | isSigned t' && not (isSigned u') -> u
        | not (isSigned t') && isSigned u' -> t
        | otherwise -> u


-- Does not throw an error on type mismatch
applyType :: Type -> AnalysisM Type
applyType (FunctionType (FunType tyReturn _ _) attrs)        = return tyReturn
applyType  (FunctionType (FunTypeIncomplete tyReturn) attrs) = return tyReturn
applyType  ty = do
  traceM $ "Warning: applying a function of type " <> tshow ty
  return simpleInt

applyIndexType :: Type -> Type -> AnalysisM Type
applyIndexType (ArrayType ty _ _ _) _ =  return ty
applyIndexType t _ = trace ("Warning: applying an index to type " <> tshow t) $ return simpleInt



-- a few shortcuts

simpleInt :: Type
simpleInt = DirectType (TyIntegral TyInt) noTypeQuals noAttributes

charType :: Type
charType = DirectType (TyIntegral TyChar) noTypeQuals noAttributes

floatType :: Type
floatType = DirectType (TyFloating TyFloat) noTypeQuals noAttributes

stringType :: Type
stringType = PtrType charType noTypeQuals noAttributes

isBooleanOp :: CBinaryOp -> Bool
isBooleanOp x = x `elem` [CGrOp, CLeOp, CLeqOp, CGeqOp, CEqOp, CNeqOp, CLndOp, CLorOp]

unsignType :: Type -> Type
unsignType (DirectType (TyIntegral intType) quals attrs) = DirectType (TyIntegral (unsignIntType intType)) quals attrs
  where
    unsignIntType TyShort  = TyUShort
    unsignIntType TyInt    = TyUInt
    unsignIntType TyInt128 = TyUInt128
    unsignIntType TyLong   = TyULong
    unsignIntType TyLLong  = TyULLong
    unsignIntType TyChar   = TyUChar
    unsignIntType t        = error $ "unsigning int type: " <> show t

integralPrecision :: IntType -> Int
integralPrecision TyBool    = 1
integralPrecision TyChar    = 8
integralPrecision TySChar   = 8
integralPrecision TyUChar   = 8
integralPrecision TyShort   = 16
integralPrecision TyUShort  = 16
integralPrecision TyInt     = 32
integralPrecision TyUInt    = 32
integralPrecision TyInt128  = 128
integralPrecision TyUInt128 = 128
integralPrecision TyLong    = 64
integralPrecision TyULong   = 64
integralPrecision TyLLong   = 128
integralPrecision TyULLong  = 128

isSigned :: IntType ->  Bool
isSigned TyBool    = False
isSigned TyChar    = False
isSigned TySChar   = True
isSigned TyUChar   = False
isSigned TyShort   = True
isSigned TyUShort  = False
isSigned TyInt     = True
isSigned TyUInt    = False
isSigned TyInt128  = True
isSigned TyUInt128 = False
isSigned TyLong    = True
isSigned TyULong   = False
isSigned TyLLong   = True
isSigned TyULLong  = False

getIntType :: Type -> IntType
getIntType (DirectType (TyIntegral ty) _ _) = ty
getIntType _ = error "getIntType on non-int type"
