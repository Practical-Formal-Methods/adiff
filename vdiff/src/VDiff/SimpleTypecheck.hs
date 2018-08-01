{-# LANGUAGE LambdaCase #-}
-- I don't care about GNU extensions, so statements are always of type void

module VDiff.SimpleTypecheck where

import           Data.Coerce
import           Language.C.Analysis.SemRep hiding (constant)
import qualified Prelude                    as P
import           VDiff.Prelude              hiding (declarator, expression,
                                             externalDeclaration,
                                             functionDefinition,
                                             translationUnit)

return' a = Right (a, [])

newtype TrivialM a = TrivialM { unTrivialM :: Either [CError] (a, [CError])}
  deriving (Functor)

instance Applicative TrivialM where
  pure x = TrivialM (Right (x, []))
  tf <*> ta = case unTrivialM tf of
                Left errs -> TrivialM $ Left errs
                Right (f, errs) -> case unTrivialM ta of
                  Left errs        -> TrivialM $ Left errs
                  Right (a, errs') -> TrivialM $ Right (f a, errs ++ errs')


instance Monad TrivialM where
  ta >>= f = case unTrivialM ta of
               Left err -> TrivialM $ Left err
               Right (a, errs) -> case unTrivialM (f a) of
                 Left err         -> TrivialM $ Left err
                 Right (b, errs') -> TrivialM $ Right (b, errs ++ errs')

check :: Typechecker
check = unTrivialM .translationUnit

-- | annotate every expression with type "int"
translationUnit :: CTranslationUnit NodeInfo -> TrivialM (CTranslationUnit SemPhase)
translationUnit (CTranslUnit declrs ni) = CTranslUnit <$> mapM externalDeclaration declrs <*> pure ni


externalDeclaration :: CExternalDeclaration NodeInfo -> TrivialM (CExternalDeclaration SemPhase)
externalDeclaration (CDeclExt declr) = CDeclExt <$> declaration declr
externalDeclaration (CFDefExt fdef ) = CFDefExt <$> functionDefinition fdef
externalDeclaration (CAsmExt blk ni) = fail "ASM blocks not implemented"

functionDefinition :: CFunctionDef NodeInfo -> TrivialM (CFunctionDef SemPhase)
functionDefinition (CFunDef specs declr oldStyleParams body ni) = do
  specs' <- mapM declarationSpecifier specs
  declr' <- declarator declr
  oldStyleParams' <- mapM declaration oldStyleParams
  body' <- statement body
  return $ CFunDef specs' declr' oldStyleParams' body' ni

declaration :: CDeclaration NodeInfo -> TrivialM (CDeclaration SemPhase)
declaration (CStaticAssert expr lit ni) = fail "static asserts not implemented"
declaration (CDecl specs initDeclrs ni) = do
  specs' <- mapM declarationSpecifier specs
  initDeclrs' <- mapM initializedDeclarator initDeclrs
  return $ CDecl specs' initDeclrs' ni


initializedDeclarator :: (Maybe (CDeclarator NodeInfo), Maybe (CInitializer NodeInfo), Maybe (CExpression NodeInfo))
  -> TrivialM (Maybe (CDeclarator SemPhase), Maybe (CInitializer SemPhase), Maybe (CExpression SemPhase))
initializedDeclarator (md, mi, me) = do
  md' <- maybeM declarator md
  mi' <- maybeM initializer mi
  me' <- maybeM expression me
  return (md', mi', me')

initializer :: CInitializer NodeInfo -> TrivialM (CInitializer SemPhase)
initializer (CInitExpr e ni) = CInitExpr <$> expression e <*> pure ni
initializer (CInitList iniList ni) = CInitList <$> mapM initPart iniList <*> pure ni
  where
    initPart :: ([CPartDesignator NodeInfo], CInitializer NodeInfo) -> TrivialM ([CPartDesignator SemPhase], CInitializer SemPhase)
    initPart (prts, init) = do
      prts' <- mapM partDesignator prts
      init' <- initializer init
      return (prts', init')
    partDesignator (CArrDesig e ni) = CArrDesig <$> expression e <*> pure ni
    partDesignator (CMemberDesig n ni) = return $ CMemberDesig n ni
    partDesignator (CRangeDesig e1 e2 ni) = CRangeDesig <$> expression e1 <*> expression e2 <*> pure ni


declarationSpecifier :: CDeclarationSpecifier NodeInfo -> TrivialM (CDeclarationSpecifier SemPhase)
declarationSpecifier (CStorageSpec sspec) = CStorageSpec <$> storageSpecifier sspec
declarationSpecifier (CTypeSpec tspec)    = CTypeSpec <$> typeSpecifier tspec
declarationSpecifier (CFunSpec fspec)     = CFunSpec <$> functionSpecifier fspec
declarationSpecifier (CTypeQual tqual)    = CTypeQual <$> typeQualifier tqual
declarationSpecifier (CAlignSpec aspec)   = CAlignSpec <$> alignmentSpecifier aspec

functionSpecifier :: CFunctionSpecifier NodeInfo -> TrivialM (CFunctionSpecifier SemPhase)
functionSpecifier (CInlineQual ni)   = return $ CInlineQual ni
functionSpecifier (CNoreturnQual ni) = return $ CNoreturnQual ni

storageSpecifier :: CStorageSpecifier NodeInfo -> TrivialM (CStorageSpecifier SemPhase)
storageSpecifier (CAuto ni)     = return $ CAuto ni
storageSpecifier (CRegister ni) = return $ CRegister ni
storageSpecifier (CStatic ni)   = return $ CStatic ni
storageSpecifier (CExtern ni)   = return $ CExtern ni
storageSpecifier (CTypedef ni)  = return $ CTypedef ni
storageSpecifier (CThread ni)   = return $ CThread ni

alignmentSpecifier :: CAlignmentSpecifier NodeInfo -> TrivialM (CAlignmentSpecifier SemPhase)
alignmentSpecifier (CAlignAsType decl ni) = CAlignAsType <$> declaration decl <*> pure ni
alignmentSpecifier (CAlignAsExpr e ni)    = CAlignAsExpr <$> expression e <*> pure ni

typeSpecifier :: CTypeSpecifier NodeInfo -> TrivialM (CTypeSpecifier SemPhase)
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

structOrUnion :: CStructureUnion NodeInfo -> TrivialM (CStructureUnion SemPhase)
structOrUnion (CStruct tag mi mdecls attrs ni) = do
  mdecls' <- maybeM (mapM declaration) mdecls
  attrs' <- mapM attribute attrs
  return $ CStruct tag mi mdecls' attrs' ni

declarator :: CDeclarator NodeInfo -> TrivialM (CDeclarator SemPhase)
declarator (CDeclr mi derivedDeclrs mAsm attrs ni) = do
  derivedDeclrs' <- mapM derivedDeclarator derivedDeclrs
  attrs' <- mapM attribute attrs
  mAsm' <- maybeM stringLiteral mAsm
  return $ CDeclr mi derivedDeclrs' Nothing attrs' ni


stringLiteral :: CStringLiteral NodeInfo -> TrivialM (CStringLiteral SemPhase)
stringLiteral (CStrLit s ni) = return $ CStrLit s ni

derivedDeclarator :: CDerivedDeclarator NodeInfo -> TrivialM (CDerivedDeclarator SemPhase)
derivedDeclarator (CPtrDeclr typequals ni) = CPtrDeclr <$> mapM typeQualifier typequals <*> pure ni
derivedDeclarator (CArrDeclr typequals arrsize ni) = CArrDeclr <$> mapM typeQualifier typequals <*> arraySize arrsize <*> pure ni
derivedDeclarator (CFunDeclr (Left ns) attrs ni) = do
  attrs' <- mapM attribute attrs
  return $ CFunDeclr (Left ns) attrs' ni

derivedDeclarator (CFunDeclr (Right (decls,b)) attrs ni) = do
  decls' <- mapM declaration decls
  attrs' <- mapM attribute attrs
  return $ CFunDeclr (Right (decls', b)) attrs' ni

typeQualifier :: CTypeQualifier NodeInfo -> TrivialM (CTypeQualifier SemPhase)
typeQualifier(CConstQual ni)    = return $ CConstQual ni
typeQualifier(CVolatQual ni)    = return $ CVolatQual ni
typeQualifier(CRestrQual ni)    = return $ CRestrQual ni
typeQualifier(CAtomicQual ni)   = return $ CAtomicQual ni
typeQualifier(CAttrQual attr)   = CAttrQual <$> attribute attr
typeQualifier(CNullableQual ni) = return $ CNullableQual ni
typeQualifier(CNonnullQual ni)  = return $ CNonnullQual ni

arraySize :: CArraySize NodeInfo -> TrivialM (CArraySize SemPhase)
arraySize (CNoArrSize b) = return $ CNoArrSize b
arraySize (CArrSize b e) = CArrSize b <$> expression e

attribute :: CAttribute NodeInfo -> TrivialM (CAttribute SemPhase)
attribute (CAttr i exprs ni) = CAttr i <$> mapM expression exprs <*> pure ni

statement :: CStatement NodeInfo -> TrivialM (CStatement SemPhase)
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


expression :: CExpression NodeInfo -> TrivialM (CExpression SemPhase)
expression (CComma exprs ni) = do
  exprs' <- mapM expression exprs
  let ty = getType (P.head exprs')
  return $ CComma exprs' (ni, ty)

expression (CAssign op e1 e2 ni) = do
  e1' <- expression e1
  e2' <- expression e2
  ty <- unify [getType e1', getType e2']
  return $ CAssign op e1' e2' (ni, ty)

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
  ty <- unifyArithmetic [getType e1', getType e2']
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
  return $ CIndex e1' e2' (ni, simpleInt) -- TODO

expression (CCall f ps ni) = do
  f' <- expression f
  ps' <- mapM expression ps
  return $ CCall f' ps' (ni, simpleInt) -- TODO

expression (CMember e n b ni) = do
  e' <- expression e
  return $ CMember e' n b (ni, simpleInt) -- TODO

expression (CVar v ni) = return $ CVar v (ni, simpleInt) -- TODO: lookup

expression (CConst const) = CConst <$> constant const -- integer, character, floating point and string constants

expression (CCompoundLit decl initList ni) = undefined  -- a C99 compound literal
expression (CGenericSelection e ml ni) = undefined  -- a C11 generic selection
expression (CStatExpr stmt ni) = do
  stmt' <- statement stmt
  return $ CStatExpr stmt' (ni, getType stmt')
expression (CLabAddrExpr n ni) = undefined  -- a GNU C address of label
expression (CBuiltinExpr builtin) = CBuiltinExpr <$> builtinThing builtin

constant :: CConstant NodeInfo -> TrivialM (CConstant SemPhase)
constant (CIntConst c ni)   = return $ CIntConst c (ni, simpleInt)
constant (CCharConst c ni)  = return $ CCharConst c (ni, charType)
constant (CFloatConst c ni) = return $ CFloatConst c (ni, floatType)
constant (CStrConst c ni)   = return $ CStrConst c (ni, stringType)


builtinThing :: CBuiltinThing NodeInfo -> TrivialM (CBuiltinThing SemPhase)
builtinThing (CBuiltinVaArg e decl ni)                 = do
  e' <- expression e
  decl' <- declaration decl
  return $ CBuiltinVaArg e' decl' ni -- TODO: those things should have a type probably
builtinThing (CBuiltinOffsetOf decl prts ni)           = undefined
builtinThing (CBuiltinTypesCompatible decl1  decl2 ni) = undefined
builtinThing (CBuiltinConvertVector e decl ni)         = undefined

-- unifies the given types
unify :: [Type] -> TrivialM Type
unify types = return $ P.head types -- TODO: Do some unification

-- unifies the given types
unifyArithmetic :: [Type] -> TrivialM Type
unifyArithmetic = unify -- TODO: Do some unification, but make sure that it is numeric


-- a few shortcuts

simpleInt :: Type
simpleInt = DirectType (TyIntegral TyInt) noTypeQuals noAttributes

charType :: Type
charType = DirectType (TyIntegral TyChar) noTypeQuals noAttributes

floatType :: Type
floatType = DirectType (TyFloating TyFloat) noTypeQuals noAttributes

stringType :: Type
stringType = PtrType charType noTypeQuals noAttributes
