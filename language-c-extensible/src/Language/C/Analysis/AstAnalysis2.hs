{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Language.C.Analysis.AstAnalysis2 where

import           Text.PrettyPrint.HughesPJ

import           Language.C.Analysis.DeclAnalysis
import           Language.C.Analysis.DefTable     (defineLabel, globalDefs,
                                                   inFileScope, lookupLabel)
import           Language.C.Analysis.SemError
import           Language.C.Analysis.SemRep
import           Language.C.Analysis.TravMonad
import           Language.C.Analysis.TypeCheck
import           Language.C.Analysis.TypeUtils

import           Language.C.Data
import           Language.C.Pretty
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants
import           Language.C.Syntax.Ops
import           Language.C.Syntax.Utils

import           Data.Generics                    hiding (Generic)




import           Control.Monad                    hiding (mapM, mapM_)
import           Data.Foldable                    (mapM_)
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Traversable                 (mapM)
import           Prelude                          hiding (mapM, mapM_)
-- After the analysis (semantic phase) we want to store additional information:
data SemPhase


type instance AnnTranslationUnit SemPhase = (NodeInfo, GlobalDecls)
type instance AnnFunctionDef SemPhase     = NodeInfo
type instance AnnAsmExt SemPhase          = NodeInfo
type instance AnnStringLiteral SemPhase   = NodeInfo
type instance AnnAsmExt SemPhase          = NodeInfo
type instance AnnFunctionDef SemPhase     = NodeInfo
type instance AnnMisc SemPhase            = NodeInfo
type instance AnnConstant SemPhase        = (NodeInfo, Type)
type instance AnnExpression SemPhase      = (NodeInfo, Type)
type instance AnnInitializer SemPhase     = NodeInfo -- TODO: Types here?
type instance AnnStatement SemPhase       = (NodeInfo, Type)













analyseAST :: (MonadTrav m) => CTranslationUnit NodeInfo -> m (CTranslationUnit SemPhase)
analyseAST (CTranslUnit decls _file_node) = do
    decls' <- mapM analyseExt decls
    -- check we are in global scope afterwards
    getDefTable >>= \dt -> unless (inFileScope dt) $
        error "Internal Error: Not in filescope after analysis"
    gld <- globalDefs <$> getDefTable
    return $ CTranslUnit decls' (_file_node, gld)


analyseExt :: (MonadTrav m) => CExternalDeclaration NodeInfo -> m (CExternalDeclaration SemPhase)
analyseExt (CAsmExt asm n) = do
  handleAsmBlock asm
  return $ CAsmExt (analyseStringLiteral asm) n

analyseExt (CFDefExt fundef) = CFDefExt <$> analyseFunDef fundef
analyseExt (CDeclExt decl ) = CDeclExt <$> analyseDecl False decl


myAnalyseVarDecl :: (MonadTrav m) => [CDeclSpec] -> CDeclr -> [CDecl] -> NodeInfo -> m VarDecl
myAnalyseVarDecl declspecs declr oldstyle_decls node_info = do
    var_decl_info <- analyseVarDecl' True declspecs declr oldstyle_decls Nothing
    let (VarDeclInfo name fun_spec storage_spec attrs ty _declr_node) = var_decl_info
    when (isNoName name) $ astError node_info "NoName in analyseFunDef"
    let ident = identOfVarName name
    -- improve incomplete type
    ty' <- improveFunDefType ty
    -- compute storage
    fun_storage <- computeFunDefStorage ident storage_spec
    let var_decl = VarDecl name (DeclAttrs fun_spec fun_storage attrs) ty'
    return var_decl
    where
    improveFunDefType (FunctionType (FunTypeIncomplete return_ty) attrs) =
      return $ FunctionType (FunType return_ty [] False) attrs
    improveFunDefType ty = return ty


analyseFunDef :: (MonadTrav m) => CFunctionDef NodeInfo -> m (CFunctionDef SemPhase)
analyseFunDef (CFunDef declspecs declr oldstyle_decls stmt node_info) = do
    -- analyse the declarator
    var_decl <- myAnalyseVarDecl declspecs declr oldstyle_decls node_info
    -- callback for declaration
    handleVarDecl False (Decl var_decl node_info)
    -- process body
    -- callback for definition
    -- handleFunDef ident (FunDef var_decl stmt' node_info) -- TODO: Change callback definitions
    declspecs' <- analyseDeclSpecs declspecs
    declr' <- analyseDeclarator declr
    oldstyle_decls <- analyseOldstyleDecls oldstyle_decls
    stmt' <- analyseFunctionBody node_info var_decl stmt
    return $ CFunDef declspecs' declr' oldstyle_decls stmt' node_info

analyseDeclSpecs :: (MonadTrav m) => [CDeclarationSpecifier NodeInfo]-> m [CDeclarationSpecifier SemPhase]
analyseDeclSpecs = mapM analyseDeclSpec

analyseDeclSpec :: (MonadTrav m) => CDeclarationSpecifier NodeInfo-> m (CDeclarationSpecifier SemPhase)
analyseDeclSpec (CStorageSpec spec) = CStorageSpec <$> analyseStorageSpec spec
analyseDeclSpec s@(CTypeSpec spec)  = CTypeSpec <$> analyseTypeSpec spec
analyseDeclSpec (CTypeQual spec)    = CTypeQual <$> analyseTypeQual spec
analyseDeclSpec (CFunSpec spec)     = CFunSpec <$> analyseFunSpec spec
analyseDeclSpec (CAlignSpec spec)   = CAlignSpec <$> analyseAlignSpec spec

-- This is basically "id". The reason we still have to deconstruct/construct is to "coerce" the type from NodeInfo to SemPhase
-- Question to myself: Can we use one of the unsafe coerces for this?
analyseStorageSpec :: (MonadTrav m ) => CStorageSpecifier NodeInfo -> m (CStorageSpecifier SemPhase)
analyseStorageSpec (CAuto ni)     = return $ CAuto ni
analyseStorageSpec (CRegister ni) = return $ CRegister ni
analyseStorageSpec (CStatic ni)   = return $ CStatic ni
analyseStorageSpec (CExtern ni)   = return $ CExtern ni
analyseStorageSpec (CTypedef ni)  = return $ CTypedef ni
analyseStorageSpec (CThread ni)   = return $ CThread ni

analyseTypeSpec :: (MonadTrav m) => CTypeSpecifier NodeInfo -> m (CTypeSpecifier SemPhase)
analyseTypeSpec (CVoidType ni)        = return $ CVoidType ni
analyseTypeSpec (CCharType ni)        = return $ CCharType ni
analyseTypeSpec (CShortType ni)       = return $ CShortType ni
analyseTypeSpec (CIntType ni)         = return $ CIntType ni
analyseTypeSpec (CLongType ni)        = return $ CLongType ni
analyseTypeSpec (CFloatType ni)       = return $ CFloatType ni
analyseTypeSpec (CFloat128Type ni)    = return $ CFloat128Type ni
analyseTypeSpec (CDoubleType ni)      = return $ CDoubleType ni
analyseTypeSpec (CSignedType ni)      = return $ CSignedType ni
analyseTypeSpec (CUnsigType ni)       = return $ CUnsigType ni
analyseTypeSpec (CBoolType ni)        = return $ CBoolType ni
analyseTypeSpec (CComplexType ni)     = return $ CComplexType ni
analyseTypeSpec (CInt128Type ni)      = return $ CInt128Type ni
analyseTypeSpec (CSUType sue ni)      = CSUType <$> analyseStructureUnion sue <*> pure ni
analyseTypeSpec (CEnumType enu ni)    = CEnumType <$> analyseEnumeration enu <*> pure ni
analyseTypeSpec (CTypeDef i ni)       = return $ CTypeDef i ni
analyseTypeSpec (CTypeOfExpr expr ni) = CTypeOfExpr <$> undefined expr <*> pure ni
analyseTypeSpec (CTypeOfType decl ni) = CTypeOfType <$> analyseDecl False decl <*> pure ni -- TODO: True or False?
analyseTypeSpec (CAtomicType decl ni) = CAtomicType <$> analyseDecl False decl <*> pure ni

analyseStructureUnion :: (MonadTrav m) => CStructureUnion NodeInfo -> m (CStructureUnion SemPhase)
analyseStructureUnion (CStruct tag mi mDecls attrs ni) = do
  mDecls' <- mapMaybeM mDecls (mapM (analyseDecl True))
  attrs' <- analyseAttrs attrs
  return $ CStruct tag mi mDecls' attrs' ni

analyseEnumeration :: (MonadTrav m ) => CEnumeration NodeInfo -> m (CEnumeration SemPhase)
analyseEnumeration (CEnum mi mitems attrs ni) = do
  items' <- case mitems of
              Nothing    -> return Nothing
              Just items -> Just <$> mapM analyseItem items
  attrs' <- analyseAttrs attrs
  return $ CEnum mi items' attrs' ni
  where
    analyseItem (i, Nothing) = return (i, Nothing)
    analyseItem (i, Just e) = do
      e' <- tExpr [] RValue e
      return (i, Just e')

analyseTypeQual :: (MonadTrav m) => CTypeQualifier NodeInfo -> m (CTypeQualifier SemPhase)
analyseTypeQual (CConstQual ni)    = return $ CConstQual ni
analyseTypeQual (CVolatQual ni)    = return $ CVolatQual ni
analyseTypeQual (CRestrQual ni)    = return $ CRestrQual ni
analyseTypeQual (CAtomicQual ni)   = return $ CAtomicQual ni
analyseTypeQual (CAttrQual attr)   = CAttrQual <$> analyseAttr attr
analyseTypeQual (CNullableQual ni) = return $ CNullableQual ni
analyseTypeQual (CNonnullQual ni)  = return $ CNonnullQual ni

analyseFunSpec :: (MonadTrav m) => CFunctionSpecifier NodeInfo -> m (CFunctionSpecifier SemPhase)
analyseFunSpec (CInlineQual ni)   = return $ CInlineQual ni
analyseFunSpec (CNoreturnQual ni) = return $ CNoreturnQual ni

analyseAlignSpec :: (MonadTrav m) => CAlignmentSpecifier NodeInfo -> m (CAlignmentSpecifier SemPhase)
analyseAlignSpec (CAlignAsExpr expr ni) = CAlignAsExpr <$> tExpr [] RValue expr <*> pure ni
analyseAlignSpec (CAlignAsType decl ni) = CAlignAsType <$> analyseDecl True decl <*> pure ni

analyseDeclarator :: (MonadTrav m)  => CDeclarator NodeInfo -> m (CDeclarator SemPhase)
analyseDeclarator (CDeclr i derivedDecl mStringLit attrs ni) = do
  derivedDecl' <- mapM analyseDerivedDeclarator derivedDecl
  let mStringLit' = analyseStringLiteral <$> mStringLit
  attrs' <- analyseAttrs attrs
  return $ CDeclr i derivedDecl' mStringLit' attrs' ni -- attrs' ni


analyseDerivedDeclarator :: (MonadTrav m) => CDerivedDeclarator NodeInfo -> m (CDerivedDeclarator SemPhase)
analyseDerivedDeclarator (CPtrDeclr tyQuals ni)       = CPtrDeclr <$> mapM analyseTypeQual tyQuals <*> pure ni
analyseDerivedDeclarator (CArrDeclr tyQuals aSize ni) = CArrDeclr <$> mapM analyseTypeQual tyQuals <*> analyseArraySize aSize <*> pure ni
analyseDerivedDeclarator (CFunDeclr (Left ids) attrs ni) = CFunDeclr (Left ids)  <$> analyseAttrs attrs <*> pure ni
analyseDerivedDeclarator (CFunDeclr (Right (decl, b)) attrs ni) = do
  decl' <- mapM (analyseDecl False) decl -- TODO: True or False?
  attrs' <- analyseAttrs attrs
  return $ CFunDeclr (Right (decl', b)) attrs' ni

analyseArraySize :: (MonadTrav m) => CArraySize NodeInfo -> m (CArraySize SemPhase)
analyseArraySize (CNoArrSize b)    = return $ CNoArrSize b
analyseArraySize (CArrSize b expr) = CArrSize b <$> tExpr [] RValue expr

analyseOldstyleDecls :: (MonadTrav m) => [CDeclaration NodeInfo]-> m [CDeclaration SemPhase]
analyseOldstyleDecls = mapM (analyseDecl False)

analyseFunctionBody :: (MonadTrav m) => NodeInfo -> VarDecl -> CStatement NodeInfo -> m (CStatement SemPhase)
analyseFunctionBody node_info decl s@(CCompound localLabels items node_info2) =
  do enterFunctionScope
     mapM_ (withDefTable . defineLabel) (localLabels ++ getLabels s)
     defineParams node_info decl
     -- record parameters
     items' <- mapM (tBlockItem [FunCtx decl]) items
     leaveFunctionScope
     return $ CCompound localLabels items' (node_info2, voidType)

analyseFunctionBody _ _ s = astError (nodeInfo s) "Function body is no compound statement"




analyseAsm :: Monad m => CAssemblyStatement NodeInfo -> m (CAssemblyStatement SemPhase)
analyseAsm = undefined



analyseStringLiteral :: CStringLiteral NodeInfo -> CStringLiteral SemPhase
analyseStringLiteral (CStrLit cStr ni) = CStrLit cStr ni

--------------------------------------------------------------------------------
-- Down here are functions I just copied over
--------------------------------------------------------------------------------

computeFunDefStorage :: (MonadTrav m) => Ident -> StorageSpec -> m Storage
computeFunDefStorage _ (StaticSpec _)  = return$ FunLinkage InternalLinkage
computeFunDefStorage ident other_spec  = do
  obj_opt <- lookupObject ident
  let defaultSpec = FunLinkage ExternalLinkage
  case other_spec of
    NoStorageSpec  -> return$ maybe defaultSpec declStorage obj_opt
    (ExternSpec False) -> return$ maybe defaultSpec declStorage obj_opt
    bad_spec -> throwTravError $ badSpecifierError (nodeInfo ident)
                  $ "unexpected function storage specifier (only static or extern is allowed)" ++ show bad_spec

defineParams :: MonadTrav m => NodeInfo -> VarDecl -> m ()
defineParams ni decl =
  case getParams (declType decl) of
    Nothing -> astError ni
               "expecting complete function type in function definition"
    Just params -> mapM_ handleParamDecl params

-- | Typecheck a block item. When statement expressions are blocks,
--   they have the type of their last expression statement, so this
--   needs to return a type.
tBlockItem :: MonadTrav m => [StmtCtx] -> CCompoundBlockItem NodeInfo -> m (CCompoundBlockItem SemPhase)
tBlockItem ctx (CBlockStmt stmt) = CBlockStmt <$> tStmt ctx stmt
tBlockItem _ (CBlockDecl d)      = CBlockDecl <$> analyseDecl True d

-- TODO: fixup analyseFunDef to handle nested functions
tBlockItem _ (CNestedFunDef fd) = do
  analyseFunDef fd >> return voidType
  undefined

-- TODO: This belongs somewhere else probably
class HasType a where
  getType :: a -> Type



instance HasType (CStatement SemPhase) where
  getType  (CLabel _ _ _ (_,ty)) = ty
  getType (CCase _ _ (_,ty))     = ty
  getType (CCases _ _ _ (_,ty))  = ty
  getType (CDefault _ (_,ty))    = ty
  getType (CExpr _ (_,ty))       = ty
  getType (CCompound _ _ (_,ty)) = ty
  getType (CIf _ _ _ (_,ty))     = ty
  getType (CSwitch _ _ (_,ty))   = ty
  getType (CWhile _ _ _ (_,ty))  = ty
  getType (CFor _ _ _ _ (_,ty))  = ty
  getType (CGoto _ (_,ty))       = ty
  getType (CGotoPtr _ (_,ty))    = ty
  getType (CCont (_,ty))         = ty
  getType (CBreak (_,ty))        = ty
  getType (CReturn _ (_,ty))     = ty
  getType (CAsm _ (_,ty))        = ty


instance HasType (CExpression SemPhase) where
  getType (CComma _ (_,ty))              = ty
  getType (CAssign _ _ _ (_,ty))         = ty
  getType (CCond _ _ _ (_,ty))           = ty
  getType (CBinary _ _ _ (_,ty))         = ty
  getType (CCast _ _ (_,ty))             = ty
  getType (CUnary _ _ (_,ty))            = ty
  getType (CSizeofExpr _ (_,ty))         = ty
  getType (CSizeofType _ (_,ty))         = ty
  getType (CAlignofExpr _ (_,ty))        = ty
  getType (CAlignofType _ (_,ty))        = ty
  getType (CComplexReal _ (_,ty))        = ty
  getType (CComplexImag _ (_,ty))        = ty
  getType (CIndex _ _ (_,ty))            = ty
  getType (CCall _ _ (_,ty))             = ty
  getType (CMember _ _ _ (_,ty))         = ty
  getType (CVar _ (_,ty))                = ty
  getType (CConst c)                     = getType c
  getType (CCompoundLit _ _ (_,ty))      = ty
  getType (CGenericSelection _ _ (_,ty)) = ty
  getType (CStatExpr _ (_,ty))           = ty
  getType (CLabAddrExpr _ (_,ty))        = ty
  getType (CBuiltinExpr b)               = getType b

instance HasType (CConstant SemPhase) where
  getType (CIntConst _ (_,ty))   = ty
  getType (CCharConst _ (_,ty))  = ty
  getType (CFloatConst _ (_,ty)) = ty
  getType (CStrConst _ (_,ty))   = ty

instance HasType (CBuiltinThing SemPhase) where
  getType = undefined



analyseAttrs :: (MonadTrav m) => [CAttribute NodeInfo] -> m [CAttribute SemPhase]
analyseAttrs = mapM analyseAttr

analyseAttr :: (MonadTrav m) => CAttribute NodeInfo -> m (CAttribute SemPhase)
analyseAttr (CAttr i es ni) = CAttr i <$> mapM (tExpr [] RValue) es <*> pure ni

-- | Typecheck a statement, given a statement context. The type of a
--   statement is usually @void@, but expression statements and blocks
--   can sometimes have other types.
tStmt :: MonadTrav m => [StmtCtx] -> CStatement NodeInfo -> m (CStatement SemPhase)
tStmt c (CLabel l stmt attrs ni)         = do
  stmt' <- tStmt c stmt
  attrs' <- analyseAttrs attrs
  return $ CLabel l stmt' attrs' (ni, getType stmt')

tStmt c (CExpr me ni)              = do
  me' <- mapMaybeM me (tExpr c RValue)
  return  $ CExpr me' (ni, maybe voidType getType me')

tStmt c (CCompound ls body ni)    =
  do enterBlockScope
     mapM_ (withDefTable . defineLabel) ls
     body' <- mapM (tBlockItem c) body
     leaveBlockScope
     -- the type is the type of the last element of the compound statement? Is that GNU C?
     -- let ty = foldl (const getType) voidType body' :: Type
     return $ CCompound ls body' (ni,voidType) -- TODO: Is this correct?

tStmt c (CIf e sthen selse ni)    = do
  e' <- checkGuard c e
  sthen' <- tStmt c sthen
  selse' <- case selse of
    Nothing  -> return Nothing
    Just els -> Just <$> tStmt c els
  return $ CIf e' sthen' selse' (ni, voidType)

tStmt c (CSwitch e stmt ni) = do
  e' <- tExpr c RValue e
  _ <- checkIntegral' ni (getType e')
  stmt' <- tStmt (SwitchCtx : c) stmt
  return $ CSwitch e' stmt' (ni, getType stmt')

tStmt c (CWhile e s dw ni) = do
  e' <- checkGuard c e
  stmt' <- tStmt (LoopCtx : c) s
  return $ CWhile e' stmt' dw (ni, getType stmt')

tStmt _ (CGoto l ni) = do
  dt <- getDefTable
  ty <- case lookupLabel l dt of
          Just _ -> return voidType
          Nothing -> typeError ni $ "undefined label in goto: " ++ identToString l
  return $ CGoto l (ni,ty)

tStmt c (CCont ni) = do
  unless (inLoop c) $ astError ni "continue statement outside of loop"
  return $ CCont (ni, voidType)

tStmt c (CBreak ni)              = do
  unless (inLoop c || inSwitch c) $ astError ni "break statement outside of loop or switch statement"
  return $ CBreak (ni, voidType)

tStmt c (CReturn (Just e) ni)    = do
  e' <- tExpr c RValue e
  rt <- case enclosingFunctionType c of
             Just (FunctionType (FunType rt _ _) _) -> return rt
             Just (FunctionType (FunTypeIncomplete rt) _) -> return rt
             Just ft -> astError ni $ "bad function type: " ++ pType ft
             Nothing -> astError ni "return statement outside function"
  case (rt, getType e') of
    -- apparently it's ok to return void from a void function?
    (DirectType TyVoid _ _, DirectType TyVoid _ _) -> return ()
    (_, t) -> assignCompatible' ni CAssignOp rt t

  return $ CReturn (Just e') (ni,getType e')

tStmt _ (CReturn Nothing ni)      = return $ CReturn Nothing (ni, voidType)

-- XXX: anything to do for assembly?
tStmt _ (CAsm asm ni)  = do
  asm' <- analyseAsm asm
  return $ CAsm asm' (ni, voidType)

tStmt c (CCase e s ni)           = do
  unless (inSwitch c) $ astError ni "case statement outside of switch statement"
  e' <- tExpr c RValue e
  checkIntegral' ni (getType e')
  s' <- tStmt c s
  return $ CCase e' s' (ni, getType s')


tStmt c (CCases e1 e2 stmt ni)      = do
  unless (inSwitch c) $ astError ni "case statement outside of switch statement"
  e1' <- tExpr c RValue e1
  checkIntegral' ni (getType e1')
  e2' <- tExpr c RValue e2
  checkIntegral' ni (getType e2')
  stmt' <- tStmt c stmt
  return $ CCases e1' e2' stmt' (ni, getType stmt')

tStmt c (CDefault stmt ni)          = do
  unless (inSwitch c) $ astError ni "default statement outside of switch statement"
  stmt' <- tStmt c stmt
  return $ CDefault stmt' (ni, getType stmt')

tStmt c (CFor i g inc stmt ni)       = do
  enterBlockScope
  i' <- case i of
    Left Nothing  -> Left <$> return Nothing
    Left (Just e) -> Left . Just <$> tExpr c RValue e
    Right d       -> Right <$> analyseDecl True d
  g' <- mapMaybeM g (checkGuard c)
  inc' <- mapMaybeM inc (tExpr c RValue)
  stmt'  <- tStmt (LoopCtx : c) stmt
  leaveBlockScope
  return $ CFor i' g' inc' stmt' (ni, voidType)


tStmt c (CGotoPtr e ni) = do
  e' <- tExpr c RValue e
  case getType e' of
    (PtrType _ _ _) -> return $ CGotoPtr e' (ni, voidType)
    _               -> typeError ni "can't goto non-pointer"


tExpr :: MonadTrav m => [StmtCtx] -> ExprSide -> CExpression NodeInfo -> m (CExpression SemPhase)
tExpr = tExpr' -- TODO: This is overly-simplified
-- tExpr c side e =
--   case nameOfNode (nodeInfo e) of
--     Just n ->
--       do dt <- getDefTable
--          case lookupType dt n of
--            Just t -> return $ t
--            Nothing ->
--              do t <- tExpr' c side e
--                 withDefTable (\dt' -> (t, insertType dt' n t))
--     Nothing -> tExpr' c side e

-- | Typecheck an expression, with information about whether it
--   appears as an lvalue or an rvalue.
tExpr' :: MonadTrav m => [StmtCtx] -> ExprSide -> CExpression NodeInfo -> m (CExpression SemPhase)
tExpr' c side (CBinary op le re ni)    =
  do when (side == LValue) $ typeError ni "binary operator as lvalue"
     le' <- tExpr c RValue le
     re' <- tExpr c RValue re
     ty <- binopType' ni op (getType le') (getType re')
     return $ CBinary op le' re' (ni,ty)

tExpr' c side (CUnary CAdrOp e ni)     = do
  when (side == LValue) $ typeError ni "address-of operator as lvalue"
  case e of
    CCompoundLit d is ni -> do
      e' <- tExpr c RValue e
      d' <- analyseDecl True d
      is' <- analyseInitializerList is
      let ty = simplePtr (getType e')
      return $ CUnary CAdrOp e' (ni, ty)
    CVar i niv -> do
      iDecl <- lookupObject i
      case iDecl of
        Nothing -> typeErrorOnLeft niv $ notFound i
        Just iDecl' -> do
          let (VarDecl _ _ ty) = getVarDecl iDecl'
          ty' <- typeErrorOnLeft niv $ maybe (notFound i) varAddrType iDecl
          return $ CUnary CAdrOp (CVar i (niv, ty)) (ni, ty')
    _        -> do
      e' <- tExpr c LValue e
      let ty = simplePtr (getType e')
      return $ CUnary CAdrOp e' (ni,simplePtr (getType e'))

tExpr' c _ (CUnary CIndOp e ni) = do
  e' <- tExpr c RValue e
  ty <- typeErrorOnLeft ni $ derefType (getType e')
  return $ CUnary CIndOp e' (ni,ty)

tExpr' c _ (CUnary CCompOp e ni) = do
  e' <- tExpr c RValue e
  checkIntegral' ni (getType e')
  return $ CUnary CCompOp e' (ni, getType e')

tExpr' c side (CUnary CNegOp e ni) = do
  when (side == LValue) $ typeError ni "logical negation used as lvalue"
  e' <- tExpr c RValue e
  checkScalar' ni (getType e')
  return $ CUnary CNegOp e' (ni, boolType)

tExpr' c side (CUnary op e ni) = do
  e' <- tExpr c (if isEffectfulOp op then LValue else side) e
  return $ CUnary op e' (ni, getType e')

tExpr' c _ (CIndex b i ni) = do
  b' <- tExpr c RValue b
  i' <- tExpr c RValue i
  addrTy <- binopType' ni CAddOp (getType b') (getType i')
  ty <- typeErrorOnLeft ni $ derefType addrTy
  return $ CIndex b' i' (ni, ty)

tExpr' c side (CCond e1 me2 e3 ni) = do
  e1' <- tExpr c RValue e1
  checkScalar' (nodeInfo e1) (getType e1')
  e3' <- tExpr c side e3
  case me2 of
    Just e2 -> do
      e2' <- tExpr c side e2
      ty <- conditionalType' ni (getType e2') (getType e3')
      return $ CCond e1' (Just e2') e3' (ni, ty)
    Nothing -> do
      ty <- conditionalType' ni (getType e1') (getType e3')
      return $ CCond e1' Nothing e3' (ni, ty)

tExpr' c _ (CMember e m deref ni) = do
  e' <- tExpr c RValue e
  ty <- if deref
        then typeErrorOnLeft ni (derefType (getType e'))
        else return (getType e')
  ty' <- fieldType ni m ty
  return $ CMember e' m deref (ni, ty')

tExpr' c side (CComma es ni) = do
  es' <- mapM (tExpr c side) es
  return $ CComma es' (ni, getType (last es'))

tExpr' c side (CCast d e ni) = do
  d' <- analyseDecl True d
  dt <- analyseTypeDecl d
  e' <- tExpr c side e
  typeErrorOnLeft ni $ castCompatible dt (getType e')
  return $ CCast d' e' (ni, dt)

tExpr' c side (CSizeofExpr e ni) = do
  when (side == LValue) $ typeError ni "sizeof as lvalue"
  e' <- tExpr c RValue e
  return $ CSizeofExpr e' (ni, size_tType)

tExpr' c side (CAlignofExpr e ni) = do
  when (side == LValue) $ typeError ni "alignof as lvalue"
  e' <- tExpr c RValue e
  return $ CAlignofExpr e' (ni, size_tType)

tExpr' c side (CComplexReal e ni) = do
  e' <- tExpr c side e
  ty <- complexBaseType ni c side e
  return $ CComplexReal e' (ni, ty)

tExpr' c side (CComplexImag e ni) = do
  e' <- tExpr c side e
  ty <- complexBaseType ni c side e
  return $ CComplexImag e' (ni, ty)

tExpr' _ side (CLabAddrExpr i ni) = do
  when (side == LValue) $ typeError ni "label address as lvalue"
  let ty = PtrType voidType noTypeQuals []
  return $ CLabAddrExpr i (ni,ty)

tExpr' _ side (CCompoundLit d initList ni) = do
  when (side == LValue) $ typeError ni "compound literal as lvalue"
  d' <- analyseDecl True d
  lt <- analyseTypeDecl d
  initList' <- tInitList ni (canonicalType lt) initList
  return $ CCompoundLit d' initList' (ni, lt)

tExpr' _ RValue (CAlignofType d ni)     = do
  d' <- analyseDecl True d
  return $ CAlignofType d' (ni, size_tType)

tExpr' _ RValue (CSizeofType d ni) = do
  enterBlockScope -- TODO: Is this the right scope?
  d' <- analyseDecl False d
  leaveBlockScope
  return $ CSizeofType d' (ni, size_tType)

tExpr' _ LValue (CAlignofType _ ni)    = typeError ni "alignoftype as lvalue"

tExpr' _ LValue (CSizeofType _ ni)     = typeError ni "sizeoftype as lvalue"

tExpr' ctx side (CGenericSelection expr list ni) = do
  error "generic selection"
  -- ty_sel <- tExpr ctx side expr
  -- ty_list <- mapM analyseAssoc list
  -- def_expr_ty <-
  --   case dropWhile (isJust . fst) ty_list of
  --     [(Nothing,tExpr'')] -> return (Just tExpr'')
  --     [] -> return Nothing
  --     _ -> astError ni "more than one default clause in generic selection"
  -- case dropWhile (maybe True (not . typesMatch ty_sel) . fst) ty_list of
  --   ((_,expr_ty) : _ ) -> return expr_ty
  --   [] -> case def_expr_ty of
  --     (Just expr_ty) -> return expr_ty
  --     Nothing -> astError ni ("no clause matches for generic selection (not fully supported) - selector type is " ++ show (pretty ty_sel) ++
  --                             ", available types are " ++ show (map (pretty.fromJust.fst) (filter (isJust.fst) ty_list)))
  -- where
  --   analyseAssoc (mdecl,expr') = do
  --     tDecl <- mapM analyseTypeDecl mdecl
  --     tExpr'' <- tExpr ctx side expr'
  --     return (tDecl, tExpr'')
  --   typesMatch (DirectType tn1 _ _) (DirectType tn2 _ _) = directTypesMatch tn1 tn2
  --   typesMatch _ _ = False -- not fully supported
  --   directTypesMatch TyVoid TyVoid                   = True
  --   directTypesMatch (TyIntegral t1) (TyIntegral t2) = t1 == t2
  --   directTypesMatch (TyFloating t1) (TyFloating t2) = t1 == t2
  --   directTypesMatch (TyComplex t1) (TyComplex t2)   = t1 == t2
  --   directTypesMatch _ _                             = False -- TODO: not fully supported
tExpr' _ _ (CVar i ni) = do
  x <- lookupObject i
  ty <- maybe (typeErrorOnLeft ni $ notFound i) (return . declType) x
  return $ CVar i (ni, ty)

tExpr' _ _ (CConst c) = do
  c' <- analyseCConstant c
  constType c
  return $ CConst c'

tExpr' _ _ (CBuiltinExpr b) =do
  b' <- analyseBuiltinThing b
  return $ CBuiltinExpr b'

tExpr' c side (CCall (CVar i _) args ni)
  | identToString i == "__builtin_choose_expr" =
    undefined
    -- case args of
    --   [g, e1, e2] ->
    --     -- XXX: the MachineDesc parameter below should be configurable
    --     do b <- constEval defaultMD Map.empty g
    --        case boolValue b of
    --          Just True -> tExpr c side e1
    --          Just False -> tExpr c side e2
    --          Nothing ->
    --            astError ni "non-constant argument to __builtin_choose_expr"
    --   _ -> astError ni "wrong number of arguments to __builtin_choose_expr"
tExpr' c _ (CCall fe args ni) =
  do let defType = FunctionType
                   (FunTypeIncomplete
                    (DirectType (TyIntegral TyInt) noTypeQuals noAttributes))
                   noAttributes
         fallback i = do warn $ invalidAST ni $
                                "unknown function: " ++ identToString i
                         return defType
     fe' <- case fe of
            CVar i ni -> do
                        x <- lookupObject i
                        case x of
                          Nothing -> do
                            ty <- fallback i
                            return $ CVar i (ni,ty)
                          Just _ -> do
                            x <- tExpr c RValue fe
                            return $ CVar i (ni, getType x)

            _ -> tExpr c RValue fe
     args' <- mapM (tExpr c RValue) args
     let atys = map getType args'
     -- XXX: we don't actually want to return the canonical return type here
     case canonicalType (getType fe') of
       PtrType (FunctionType (FunType rt pdecls varargs) _) _ _ ->
         do let ptys = map declType pdecls
            mapM_ checkArg $ zip3 ptys atys args
            unless varargs $ when (length atys /= length ptys) $
                   typeError ni "incorrect number of arguments"
            return $ CCall fe' args' (ni, canonicalType rt)
       PtrType (FunctionType (FunTypeIncomplete rt) _) _ _ ->
         do -- warn $ invalidAST ni "incomplete function type"
            return $ CCall fe' args' (ni, canonicalType rt)
       _  -> typeError ni $ "attempt to call non-function of type " ++ pType (getType fe')
  where checkArg (pty, aty, arg) =
          do attrs <- deepTypeAttrs pty
             if isTransparentUnion attrs
               then
                 case canonicalType pty of
                   DirectType (TyComp ctr) _ _ ->
                     do td <- lookupSUE (nodeInfo arg) (sueRef ctr)
                        _ms <- tagMembers (nodeInfo arg) td
                        {-
                        when (null $ rights $ matches ms) $
                             astError (nodeInfo arg) $
                             "argument matches none of the elements " ++
                             "of transparent union"
                        -}
                        return ()
                     -- where matches =
                     --         map (\d -> assignCompatible
                     --                    CAssignOp
                     --                    (snd d)
                     --                    aty
                     --             )
                   _ -> astError (nodeInfo arg)
                        "non-composite has __transparent_union__ attribute"
               else
                 assignCompatible' (nodeInfo arg) CAssignOp pty aty
        isTransparentUnion =
          any (\(Attr n _ _) -> identToString n == "__transparent_union__")
tExpr' c _ (CAssign op le re ni) = do
  le' <- tExpr c LValue le
  re' <- tExpr c RValue re
  when (constant $ typeQuals (getType le')) $
      typeError ni $ "assignment to lvalue with `constant' qualifier: "
                      ++ (render . pretty) le
  case (canonicalType (getType le'), re) of
    (lt', CConst (CIntConst i _))
      | isPointerType lt' && getCInteger i == 0 -> return ()
    (_, _) -> assignCompatible' ni op (getType le') (getType re')
  return $ CAssign op le' re' (ni, getType le')

tExpr' c _ (CStatExpr s ni) = do
  enterBlockScope
  mapM_ (withDefTable . defineLabel) (getLabels s)
  s' <- tStmt c s
  leaveBlockScope
  return $ CStatExpr s' (ni, getType s')


-- | Analyse a declaration other than a function definition
--
--   Note: static assertions are not analysed
analyseDecl :: (MonadTrav m) => Bool -> CDeclaration NodeInfo -> m (CDeclaration SemPhase)
analyseDecl _is_local (CStaticAssert _expr _strlit _annot) = undefined -- return () -- TODO
analyseDecl is_local decl@(CDecl declspecs declrs node)
    | null declrs =
        case typedef_spec of Just _  -> astError node "bad typedef declaration: missing declarator"
                             Nothing -> do
                               analyseTypeDecl decl
                               declspecs' <- mapM analyseDeclSpec declspecs
                               return $ CDecl declspecs' [] node
    | (Just declspecs') <- typedef_spec = do
        declspecs'' <- mapM analyseDeclSpec declspecs'
        x <- mapM (uncurry (analyseTyDef declspecs')) declr_list
        return $ (CDecl declspecs'' x node :: CDeclaration SemPhase)
    | otherwise   = do let (storage_specs, attrs, typequals, typespecs, funspecs, _alignspecs) = partitionDeclSpecs declspecs
                       canonTySpecs <- canonicalTypeSpec typespecs
                       -- TODO: alignspecs not yet processed
                       let specs = (storage_specs, attrs, typequals, canonTySpecs, funspecs)
                       declrs' <- mapM (uncurry (analyseVarDeclr specs)) declr_list -- :: m [(Maybe (CDeclarator SemPhase), Maybe (CInitializer SemPhase), Maybe (CExpression SemPhase))]
                       declspecs' <- mapM analyseDeclSpec declspecs
                       return $ CDecl declspecs' declrs' node
    where
    declr_list = zip (True : repeat False) declrs
    typedef_spec = hasTypeDef declspecs

    analyseTyDef ::  (MonadTrav m) => [CDeclSpec]  -> Bool
      -> (Maybe CDeclr, Maybe (CInitializer NodeInfo), Maybe (CExpression NodeInfo))
      -> m (Maybe (CDeclarator SemPhase), Maybe (CInitializer SemPhase), Maybe (CExpression SemPhase))
    analyseTyDef declspecs' handle_sue_def declr =
        case declr of
            (Just tydeclr, Nothing , Nothing) -> do
              tydeclr' <- analyseTypeDef handle_sue_def declspecs' tydeclr node
              return (Just tydeclr', Nothing, Nothing)
            _ -> astError node "bad typdef declaration: bitfieldsize or initializer present"
    analyseVarDeclr :: (MonadTrav m) => ([CStorageSpec],[CAttr],[CTypeQual],TypeSpecAnalysis ,[CFunSpec]) -> Bool -> (Maybe CDeclr, Maybe Initializer, Maybe a) -> m (Maybe (CDeclarator SemPhase), Maybe (CInitializer SemPhase), Maybe (CExpression SemPhase))
    analyseVarDeclr specs handle_sue_def (Just declr, mInit, Nothing) = do
        -- analyse the declarator
        let (storage_specs, attrs, typequals, canonTySpecs, inline) = specs
        vardeclInfo@(VarDeclInfo _ _ _ _ typ _) <- analyseVarDecl handle_sue_def storage_specs attrs typequals canonTySpecs inline declr [] Nothing
        -- declare / define the object
        if isFunctionType typ
            then extFunProto vardeclInfo
            else (if is_local then localVarDecl else extVarDecl)
                 -- XXX: if Initializer becomes different from CInit, this
                 -- will have to change.
                 vardeclInfo mInit
        mInit' <- mapMaybeM mInit (tInit typ)
        declr <- analyseDeclarator declr
        return (Just declr, mInit', Nothing)
    analyseVarDeclr _ _ (Nothing,_,_)         = astError node "abstract declarator in object declaration"
    analyseVarDeclr _ _ (_,_,Just _bitfieldSz) = astError node "bitfield size in object declaration"


-- | Analyse a typedef
analyseTypeDef :: (MonadTrav m) => Bool -> [CDeclSpec] -> CDeclr -> NodeInfo -> m (CDeclarator SemPhase)
analyseTypeDef handle_sue_def declspecs declr node_info = do
    -- analyse the declarator
    (VarDeclInfo name fun_attrs storage_spec attrs ty _node) <- analyseVarDecl' handle_sue_def declspecs declr [] Nothing
    checkValidTypeDef fun_attrs storage_spec attrs
    when (isNoName name) $ astError node_info "NoName in analyseTypeDef"
    let ident = identOfVarName name
    handleTypeDef (TypeDef ident ty attrs node_info)
    analyseDeclarator declr
    where
    checkValidTypeDef fun_attrs  _ _ | fun_attrs /= noFunctionAttrs =
                                         astError node_info "inline specifier for typeDef"
    checkValidTypeDef _ NoStorageSpec _ = return ()
    checkValidTypeDef _ bad_storage _ = astError node_info $ "storage specified for typeDef: " ++ show bad_storage

checkGuard :: MonadTrav m => [StmtCtx] -> CExpression NodeInfo -> m (CExpression SemPhase)
checkGuard c e = do
  e' <- tExpr c RValue e
  checkScalar' (nodeInfo e) (getType e')
  return e'





data StmtCtx = FunCtx VarDecl
             | LoopCtx
             | SwitchCtx

data ExprSide = LValue | RValue
                deriving (Eq, Show)

-- | Given a context, determine the type declaration for the enclosing
--   function, if possible, given a context.
enclosingFunctionType :: [StmtCtx] -> Maybe Type
enclosingFunctionType []              = Nothing
enclosingFunctionType (FunCtx vd : _) = Just $ declType vd
enclosingFunctionType (_ : cs)        = enclosingFunctionType cs

complexBaseType :: MonadTrav m => NodeInfo -> [StmtCtx] -> ExprSide -> CExpr -> m Type
complexBaseType ni c side e = do
  e' <- tExpr c side e
  let t = getType e'
  case canonicalType t of
    DirectType (TyComplex ft) quals attrs ->
      return $ DirectType (TyFloating ft) quals attrs
    _ -> typeError ni $ "expected complex type, got: " ++ pType t


inLoop :: [StmtCtx] -> Bool
inLoop c = any isLoop c
  where isLoop LoopCtx = True
        isLoop _       = False

inSwitch :: [StmtCtx] -> Bool
inSwitch c = any isSwitch c
  where isSwitch SwitchCtx = True
        isSwitch _         = False

analyseBuiltinThing :: MonadTrav m => CBuiltinThing NodeInfo -> m (CBuiltinThing SemPhase)
analyseBuiltinThing = undefined

analyseCConstant :: MonadTrav m => CConstant NodeInfo -> m (CConstant SemPhase)
analyseCConstant c@(CIntConst n ni) = do
  ty <- constType c
  return $ CIntConst n (ni,ty)
analyseCConstant c@(CCharConst cchar ni) = do
  ty <- constType c
  return $ CCharConst cchar (ni,ty)
analyseCConstant c@(CFloatConst f ni) = do
  ty <- constType c
  return $ CFloatConst f (ni,ty)
analyseCConstant c@(CStrConst s ni) = do
  ty <- constType c
  return $ CStrConst s (ni,ty)

analyseInitializerList :: MonadTrav m => CInitializerList NodeInfo -> m (CInitializerList SemPhase)
analyseInitializerList  = mapM f
  where f (pds, ini) = do
          pds' <- mapM analysePartDesignator pds
          ini' <- analyseInitializer ini
          return (pds', ini')

analyseInitializer :: (MonadTrav m) => CInitializer NodeInfo -> m (CInitializer SemPhase)
analyseInitializer (CInitExpr e node) = do
  e' <- tExpr' [] RValue e
  return $ CInitExpr e' node
analyseInitializer (CInitList l node) = do
  l' <- analyseInitializerList l
  return $ CInitList l' node
  

analysePartDesignator :: MonadTrav m => CPartDesignator NodeInfo -> m (CPartDesignator SemPhase)
analysePartDesignator = undefined

-- | handle a function prototype
extFunProto :: (MonadTrav m) => VarDeclInfo -> m ()
extFunProto (VarDeclInfo var_name fun_spec storage_spec attrs ty node_info) =
    do  when (isNoName var_name) $ astError node_info "NoName in extFunProto"
        old_fun <- lookupObject (identOfVarName var_name)
        checkValidSpecs
        let decl = VarDecl var_name (DeclAttrs fun_spec (funDeclLinkage old_fun) attrs) ty
        handleVarDecl False (Decl decl node_info)
        -- XXX: structs should be handled in 'function prototype scope' too
        enterPrototypeScope
        maybe (return ()) (mapM_ handleParamDecl) (getParams ty)
        leavePrototypeScope
    where
    funDeclLinkage old_fun =
        case storage_spec of
            NoStorageSpec    -> FunLinkage ExternalLinkage -- prototype declaration / external linkage
            StaticSpec False -> FunLinkage InternalLinkage -- prototype declaration / internal linkage
            ExternSpec False -> case old_fun of
                                    Nothing -> FunLinkage ExternalLinkage
                                    Just f  -> declStorage f
            _ -> error $ "funDeclLinkage: " ++ show storage_spec
    checkValidSpecs
        | hasThreadLocalSpec storage_spec = astError node_info "thread local storage specified for function"
        | RegSpec <- storage_spec         = astError node_info "invalid `register' storage specified for function"
        | otherwise                       = return ()

-- | handle a object declaration \/ definition
--
-- We have to check the storage specifiers here, as they determine wheter we're dealing with decalartions
-- or definitions
-- see [http://www.sivity.net/projects/language.c/wiki/ExternalDefinitions]
extVarDecl :: (MonadTrav m) => VarDeclInfo -> Maybe Initializer -> m ()
extVarDecl (VarDeclInfo var_name fun_spec storage_spec attrs typ node_info) init_opt =
    do when (isNoName var_name) $ astError node_info "NoName in extVarDecl"
       (storage,is_def) <- globalStorage storage_spec
       let vardecl = VarDecl var_name (DeclAttrs fun_spec storage attrs) typ
       if is_def
           then handleObjectDef False ident $ ObjDef vardecl init_opt node_info
           else handleVarDecl False $ Decl vardecl node_info
    where
       ident = identOfVarName var_name
       globalStorage _ | fun_spec /= noFunctionAttrs =
                           astError node_info "invalid function specifier for external variable"
       globalStorage AutoSpec      = astError node_info "file-scope declaration specifies storage `auto'"
       globalStorage RegSpec       =
         do when (isJust init_opt) $ astError node_info "initializer given for global register variable"
            case var_name of
              NoName -> astError node_info "global register variable has no name"
              VarName _ Nothing -> astError node_info "no register specified for global register variable"
              _ -> return ()
            dt <- getDefTable
            when (hasFunDef dt) $ astError node_info "global register variable appears after a function definition"
            return (Static InternalLinkage False, False)
       -- tentative if there is no initializer, external
       globalStorage NoStorageSpec = return (Static ExternalLinkage False, True)
       globalStorage ThreadSpec    = return (Static ExternalLinkage True, True)
       -- tentative if there is no initializer, internal
       globalStorage (StaticSpec thread_local) = return (Static InternalLinkage thread_local, True)
       globalStorage (ExternSpec thread_local) =
           case init_opt of
               -- declaration with either external or old storage
               Nothing -> do old_decl <- lookupObject ident
                             return (maybe (Static ExternalLinkage thread_local) declStorage old_decl,False)
               -- warning, external definition
               Just _  -> do warn $ badSpecifierError node_info "Both initializer and `extern` specifier given - treating as definition"
                             return (Static ExternalLinkage thread_local, True)
       hasFunDef dt = any (isFuncDef . snd) (Map.toList $ gObjs $ globalDefs dt)
       isFuncDef (FunctionDef fd) = not $ (isInline . functionAttrs) fd
       isFuncDef _                = False

-- | handle a function-scope object declaration \/ definition
-- see [http://www.sivity.net/projects/language.c/wiki/LocalDefinitions]
localVarDecl :: (MonadTrav m) => VarDeclInfo -> Maybe Initializer -> m ()
localVarDecl (VarDeclInfo var_name fun_attrs storage_spec attrs typ node_info) init_opt =
    do when (isNoName var_name) $ astError node_info "NoName in localVarDecl"
       (storage,is_def) <- localStorage storage_spec
       let vardecl = VarDecl var_name (DeclAttrs fun_attrs storage attrs) typ
       if is_def
           then handleObjectDef True ident (ObjDef vardecl init_opt node_info)
           else handleVarDecl True (Decl vardecl node_info)
    where
    ident = identOfVarName var_name
    localStorage NoStorageSpec = return (Auto False,True)
    localStorage ThreadSpec    = return (Auto True,True)
    localStorage RegSpec = return (Auto True,True)
    -- static no linkage
    localStorage (StaticSpec thread_local) =
      return (Static NoLinkage thread_local,True)
    localStorage (ExternSpec thread_local)
      | isJust init_opt = astError node_info "extern keyword and initializer for local"
      | otherwise =
          do old_decl <- lookupObject ident
             return (maybe (Static ExternalLinkage thread_local) declStorage old_decl,False)
    localStorage _ = astError node_info "bad storage specifier for local"

tInit :: MonadTrav m => Type -> CInitializer NodeInfo-> m (CInitializer SemPhase)
tInit t i@(CInitExpr e ni) = do
  e' <- tExpr [] RValue e
  assignCompatible' ni CAssignOp t (getType e')
  return $ CInitExpr e' ni

tInit t i@(CInitList initList ni) = do
  initList' <- tInitList ni (canonicalType t) initList
  return $ CInitList initList' ni


tInitList :: MonadTrav m => NodeInfo -> Type -> CInitializerList NodeInfo-> m (CInitializerList SemPhase)
tInitList _ (ArrayType t@(DirectType (TyIntegral TyChar) _ _) _ _ _)
              [([], CInitExpr e@(CConst (CStrConst _ _)) x)] = do
  e' <- tExpr [] RValue e
  return [([], CInitExpr e' x)]
tInitList _ _ l = analyseInitializerList l
-- tInitList _ _ initList  = error "tInitList not implemented yet" -- return $ unsafeCoerce initList -- Meh
-- tInitList ni t@(ArrayType _ _ _ _) initList =
--   do let default_ds =
--            repeat (CArrDesig (CConst (CIntConst (cInteger 0) ni)) ni)
--      checkInits t default_ds initList
-- tInitList ni t@(DirectType (TyComp ctr) _ _) initList =
--   do td <- lookupSUE ni (sueRef ctr)
--      ms <- tagMembers ni td
--      let default_ds = map (\m -> CMemberDesig (fst m) ni) ms
--      checkInits t default_ds initList
-- tInitList _ (PtrType (DirectType TyVoid _ _) _ _ ) _ =
--           return () -- XXX: more checking
-- tInitList _ t [([], i)] = voidM$ tInit t i
-- tInitList ni t _ = typeError ni $ "initializer list for type: " ++ pType t


-- return @Just declspecs@ without @CTypedef@ if the declaration specifier contain @typedef@
hasTypeDef :: [CDeclSpec] -> Maybe [CDeclSpec]
hasTypeDef declspecs =
    case foldr hasTypeDefSpec (False,[]) declspecs of
        (True,specs') -> Just specs'
        (False,_)     -> Nothing
    where
    hasTypeDefSpec (CStorageSpec (CTypedef _)) (_,specs) = (True, specs)
    hasTypeDefSpec spec (b,specs)                        = (b,spec:specs)

-- (private) Get parameters of a function type
getParams :: Type -> Maybe [ParamDecl]
getParams (FunctionType (FunType _ params _) _) = Just params
getParams _                                     = Nothing

maybeM  :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f ma = maybe (return Nothing) (\x -> Just <$> f x) ma

--------------------------------------------------------------------------------
-- derived instances
deriving instance Show SemPhase
deriving instance Show (CAlignmentSpecifier SemPhase)
deriving instance Show (CArraySize SemPhase)
deriving instance Show (CAssemblyOperand SemPhase)
deriving instance Show (CAssemblyStatement SemPhase)
deriving instance Show (CAttribute SemPhase)
deriving instance Show (CBuiltinThing SemPhase)
deriving instance Show (CCompoundBlockItem SemPhase)
deriving instance Show (CDeclaration SemPhase)
deriving instance Show (CDeclarationSpecifier SemPhase)
deriving instance Show (CDeclarator SemPhase)
deriving instance Show (CDerivedDeclarator SemPhase)
deriving instance Show (CEnumeration SemPhase)
deriving instance Show (CExpression SemPhase)
deriving instance Show (CExternalDeclaration SemPhase)
deriving instance Show (CFunctionDef SemPhase)
deriving instance Show (CInitializer SemPhase)
deriving instance Show (CPartDesignator SemPhase)
deriving instance Show (CStatement SemPhase)
deriving instance Show (CStructureUnion SemPhase)
deriving instance Show (CTypeQualifier SemPhase)
deriving instance Show (CTypeSpecifier SemPhase)
deriving instance Show (CStringLiteral SemPhase)
deriving instance Show (CStorageSpecifier SemPhase)
deriving instance Show (CConstant SemPhase)
deriving instance Show (CFunctionSpecifier SemPhase)


deriving instance Data SemPhase
deriving instance Data (CAlignmentSpecifier SemPhase)
deriving instance Data (CArraySize SemPhase)
deriving instance Data (CAssemblyOperand SemPhase)
deriving instance Data (CAssemblyStatement SemPhase)
deriving instance Data (CAttribute SemPhase)
deriving instance Data (CBuiltinThing SemPhase)
deriving instance Data (CCompoundBlockItem SemPhase)
deriving instance Data (CDeclaration SemPhase)
deriving instance Data (CDeclarationSpecifier SemPhase)
deriving instance Data (CDeclarator SemPhase)
deriving instance Data (CDerivedDeclarator SemPhase)
deriving instance Data (CEnumeration SemPhase)
deriving instance Data (CExpression SemPhase)
deriving instance Data (CExternalDeclaration SemPhase)
deriving instance Data (CFunctionDef SemPhase)
deriving instance Data (CInitializer SemPhase)
deriving instance Data (CPartDesignator SemPhase)
deriving instance Data (CStatement SemPhase)
deriving instance Data (CStructureUnion SemPhase)
deriving instance Data (CTypeQualifier SemPhase)
deriving instance Data (CTypeSpecifier SemPhase)
deriving instance Data (CStringLiteral SemPhase)
deriving instance Data (CStorageSpecifier SemPhase)
deriving instance Data (CConstant SemPhase)
deriving instance Data (CFunctionSpecifier SemPhase)
deriving instance Data (CTranslationUnit SemPhase)

deriving instance Eq SemPhase
deriving instance Eq (CAlignmentSpecifier SemPhase)
deriving instance Eq (CArraySize SemPhase)
deriving instance Eq (CAssemblyOperand SemPhase)
deriving instance Eq (CAssemblyStatement SemPhase)
deriving instance Eq (CAttribute SemPhase)
deriving instance Eq (CBuiltinThing SemPhase)
deriving instance Eq (CCompoundBlockItem SemPhase)
deriving instance Eq (CDeclaration SemPhase)
deriving instance Eq (CDeclarationSpecifier SemPhase)
deriving instance Eq (CDeclarator SemPhase)
deriving instance Eq (CDerivedDeclarator SemPhase)
deriving instance Eq (CEnumeration SemPhase)
deriving instance Eq (CExpression SemPhase)
deriving instance Eq (CExternalDeclaration SemPhase)
deriving instance Eq (CFunctionDef SemPhase)
deriving instance Eq (CInitializer SemPhase)
deriving instance Eq (CPartDesignator SemPhase)
deriving instance Eq (CStatement SemPhase)
deriving instance Eq (CStructureUnion SemPhase)
deriving instance Eq (CTypeQualifier SemPhase)
deriving instance Eq (CTypeSpecifier SemPhase)
deriving instance Eq (CStringLiteral SemPhase)
deriving instance Eq (CStorageSpecifier SemPhase)
deriving instance Eq (CConstant SemPhase)
deriving instance Eq (CFunctionSpecifier SemPhase)
-- deriving instance Eq (CTranslationUnit SemPhase)

deriving instance Ord SemPhase
deriving instance Ord (CAlignmentSpecifier SemPhase)
deriving instance Ord (CArraySize SemPhase)
deriving instance Ord (CAssemblyOperand SemPhase)
deriving instance Ord (CAssemblyStatement SemPhase)
deriving instance Ord (CAttribute SemPhase)
deriving instance Ord (CBuiltinThing SemPhase)
deriving instance Ord (CCompoundBlockItem SemPhase)
deriving instance Ord (CDeclaration SemPhase)
deriving instance Ord (CDeclarationSpecifier SemPhase)
deriving instance Ord (CDeclarator SemPhase)
deriving instance Ord (CDerivedDeclarator SemPhase)
deriving instance Ord (CEnumeration SemPhase)
deriving instance Ord (CExpression SemPhase)
deriving instance Ord (CExternalDeclaration SemPhase)
deriving instance Ord (CFunctionDef SemPhase)
deriving instance Ord (CInitializer SemPhase)
deriving instance Ord (CPartDesignator SemPhase)
deriving instance Ord (CStatement SemPhase)
deriving instance Ord (CStructureUnion SemPhase)
deriving instance Ord (CTypeQualifier SemPhase)
deriving instance Ord (CTypeSpecifier SemPhase)
deriving instance Ord (CStringLiteral SemPhase)
deriving instance Ord (CStorageSpecifier SemPhase)
deriving instance Ord (CConstant SemPhase)
deriving instance Ord (CFunctionSpecifier SemPhase)
-- deriving instance Ord (CTranslationUnit SemPhase)
