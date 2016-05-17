{-# LANGUAGE TupleSections #-}
module KindLang.Analysis.ResolveTypes where

import Data.List
import Data.Maybe
import Debug.Trace
import Control.Monad
import Control.Monad.Except
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Catalogue
import KindLang.Data.Error
import KindLang.Data.Types
import KindLang.Data.KStat
import qualified KindLang.Locale.ErrorMessages as ErrorMessages
import KindLang.Data.Scope
import KindLang.Analysis.BuildCatalogue
import KindLang.Lib.CoreTypes
import KindLang.Lib.Operators

-- FIXME modules with two functions and foreword references between them
-- are not correctly resolving.

-- | Return a copy of a module tree with all types resolved, or an error message.
resolveModule :: Module -> Scope -> ModuleLoader s -> KStat s Module
resolveModule (Module name imports deflist) s ldr = do
    typeResolved <- resolveDefListTypes s deflist
    catalogues <- buildCatalogues ldr (Module name imports typeResolved)
    let moduleScope = (makeModuleScope s catalogues)
    fullyResolved <- resolveFunctionInstances moduleScope typeResolved
    return $ Module name imports fullyResolved

-- | Return a copy of a definition list resolved against a given scope.
resolveDefListTypes :: Scope -> DefList -> KStat s DefList
resolveDefListTypes s =
    mapM resolve
    where
      resolve :: (String,Definition) -> KStat s (String,Definition)
      resolve (cid,def) = --trace ("resolve: " ++ cid)
                          (cid,) <$> resolveDefinition s def

-- | Scans a definition list for functions (including functions inside
-- classes) and resolves their instance bodies.
resolveFunctionInstances :: Scope -> DefList -> KStat s DefList
resolveFunctionInstances s =
    mapM resolveInstances
    where
      resolveInstances (cid, def) = (cid,) <$> resolveImplementation s def

-- | Performs second pass of resolution, fixing up implementations
-- that were skipped when resolveDefinition was used in the first pass.
resolveImplementation :: Scope -> Definition -> KStat s Definition
resolveImplementation s (FunctionDefinition instances) = do
    resolvedInstances <- mapM (resolveInstance s) instances
    return $ FunctionDefinition resolvedInstances
-- fixme scan classes for memmbers
resolveImplementation _ other = trace ("resolveImplementation:" ++ show other) $ return other

-- | Return a copy of a class member definition list resolved against a
-- given scope.
resolveClassDefListTypes :: Scope -> [ClassMember] -> KStat s [ClassMember]
resolveClassDefListTypes s = mapM (resolveClassMember s)

-- | Resolve an individual class member definition against a given scope.
resolveClassMember :: Scope -> ClassMember -> KStat s ClassMember
resolveClassMember s (ClassMember n v d) =
    ClassMember n v <$> resolveDefinition s d

-- | Resolve an individual definition against a given scope.
resolveDefinition :: Scope -> Definition -> KStat s Definition 
resolveDefinition s (VariableDefinition urt@(SimpleType _) i) = do
    -- fixme how do we use resolveType here?
    -- fixme if 'i' is an init expression it should be resolved & typechecked.
    -- fixme if 'i' is a constructor expression it should be resolved.
    rt <- typeLookup s urt
    return $ VariableDefinition rt i
resolveDefinition s (VariableDefinition InferableType (VarInitExpr e)) = do
    ae <- resolveExpr s e
    return $ VariableDefinition (aexprType ae) (VarInitAExpr ae)
resolveDefinition s (ClassDefinition cdlist) =
    ClassDefinition <$> resolveClassDefListTypes s cdlist
resolveDefinition s (FunctionDefinition instances) =
    FunctionDefinition <$> mapM (resolveInstanceTypes s) instances
resolveDefinition _ nonMatching = return nonMatching

-- | Resolve an expression tree against a given scope, returning a resolved
-- copy (an 'AExpr' with the same meaning as the unresolved 'Expr') or an error.
resolveExpr :: Scope -> Expr -> KStat s AExpr
-- pre-annotated expressions can simply be returned
resolveExpr _ (Annotated aexpr) = return aexpr
-- literals have predefined types
resolveExpr _ (IntLiteral v) = return $ AIntLiteral eaKindInt v
resolveExpr _ (StringLiteral v) = return $ AStringLiteral eaKindString v
-- variable and object references
resolveExpr s (VarRef sid) =
    scopeLookup s sid >>=
      identDefToExprAnnotation >>=
        \ ann -> return $ AVarRef ann sid
resolveExpr s (ORef oexpr sid) = do
    aoexpr <- resolveExpr s oexpr
    (cid, refType) <- resolveTypeRef s (aexprType aoexpr) sid
    return $ AORef (crefAnnotation cid refType) aoexpr sid
-- operators
resolveExpr s (BinOp operator l r) = do
    al <- resolveExpr s l
    ar <- resolveExpr s r
    operatorDef <- findBinaryOperator operator (aexprType al) (aexprType ar)
    annotation <- makeFunctionCallAnnotation
                    (aexprType operatorDef)
                    ([(aexprType al), (aexprType ar)])
    return $ AFunctionApplication annotation operatorDef [al, ar]
resolveExpr s (PrefixOp operator e) = do
    ae <- resolveExpr s e
    operatorDef <- findPrefixOperator operator (aexprType ae)
    annotation <- makeFunctionCallAnnotation
                    (aexprType operatorDef)
                    [(aexprType ae)]
    return $ AFunctionApplication annotation operatorDef [ae]
-- function application
resolveExpr s (FunctionApplication fnExpr paramExprs) = do
    rFnExpr <- resolveExpr s fnExpr
    rParamExprs <- sequence $ fmap (resolveExpr s) paramExprs
    annotation <- makeFunctionCallAnnotation
                    (aexprType rFnExpr)
                    (aexprType <$> rParamExprs)
    return $ AFunctionApplication annotation rFnExpr rParamExprs
-- object method application
resolveExpr s (OMethod obExpr sid paramExprs) = do
    aObExpr <- resolveExpr s obExpr
    (cid, methodType) <- resolveTypeRef s (aexprType aObExpr) sid
    rParams <- sequence $ fmap (resolveExpr s) paramExprs
    annotation <- makeFunctionCallAnnotation methodType (aexprType <$> rParams)
    return $ AOMethod annotation aObExpr methodType cid rParams
    
-- fixme - this function should be handled by the scope the id is found in
-- as different scope types should be able to produce different annotations.

-- | Utility function to build an annotation for a reference operation
crefAnnotation :: NSID -> TypeDescriptor -> ExprAnnotation
crefAnnotation cid t = (ExprAnnotation t [("CanonicalID", EADId cid)])

-- | Build an appropriate expression annotation for an expression that
-- refers to the object defined with the definition.  The definition
-- must be fully resolved and must refer to an object that has a
-- referencable value.  Returns an annotation or an error on failure.
identDefToExprAnnotation :: IdentDefinition -> KStat s ExprAnnotation
identDefToExprAnnotation (cid, VariableDefinition (Reference t) x) =
    identDefToExprAnnotation (cid, VariableDefinition t x)
identDefToExprAnnotation (cid, VariableDefinition rt@(ResolvedType _ _ _) _) =
    return $ crefAnnotation cid (Reference rt)
identDefToExprAnnotation (cid, VariableDefinition rt _) =
    throwError $ InternalError (nsidString cid ++ " is not resolved (" ++
                                         show rt ++")")
identDefToExprAnnotation (cid, FunctionDefinition []) =
    throwError $ InternalError (nsidString cid ++ " contains no instances")
identDefToExprAnnotation (cid, FunctionDefinition (fnInstance:[])) =
    return $ crefAnnotation cid (fnInstanceType fnInstance)
identDefToExprAnnotation (_, FunctionDefinition _) =
    error "overloaded functions not implemented"
identDefToExprAnnotation (cid, def) =
    throwError $ TypeError cid ("referenced as a variable but is a " ++
                          (definitionTypeName def))

-- | @resolveTypeRef s desc sid@ returns the canonical identifier and type
-- descriptor of an item whose identifier is @sid@ residing inside an object
-- of type @desc@ refered to in scope @s@, or an error message if no such
-- object can be resolved.
resolveTypeRef :: Scope -> TypeDescriptor -> NSID ->
                  KStat s (Identified TypeDescriptor)
resolveTypeRef _ (ResolvedType _ cid (ClassDefinition members))
                 sid@(UnqualifiedID memberId) = 
    -- fixme may be better if classdef has a map rather than a list?
    case find ((== memberId) . classMemberName) members of
      Nothing -> throwError $ IdentifierNotFound $ fqid
      Just (ClassMember _ Public def) -> do
          (ExprAnnotation rt _) <- identDefToExprAnnotation (fqid, def)
          return (fqid, rt)
      Just (ClassMember _ access _) -> throwError $ AccessViolation fqid access
    where
      fqid = sid `qualifiedBy` cid
    -- fixme what to do with qualified member references?
resolveTypeRef _ (ResolvedType _ cid def) sid =
    throwError $ TypeError (sid `qualifiedBy` cid)
             ((nsidString sid) ++ " is a " ++ (definitionTypeName def))
resolveTypeRef s (Reference t) sid = resolveTypeRef s t sid
resolveTypeRef _ t _ =
    throwError $ InternalError ("dereferenced object is not resolved (" ++
                          show t ++ ")")

-- fixme how should overloaded functions work?
-- | Given a function type and the types of arguments passed to it, build
-- an oppropriate annotation for the result of applying the arguments to the
-- function, or an error message if the application is malformed.
makeFunctionCallAnnotation :: TypeDescriptor -> [TypeDescriptor] ->
                              KStat s ExprAnnotation
makeFunctionCallAnnotation (FunctionType fml res) actl
    | typesCompatible fml actl = return $ ExprAnnotation res []
    | otherwise                = throwError $ InvalidApplication fml actl
makeFunctionCallAnnotation (ForAllTypes tlist _
                                        ftype@(FunctionType formal _))
                           actual =
    do
      let substitutions :: [(String,TypeDescriptor)]
          substitutions = catMaybes $ map
                             (uncurry generateSubstitution)
                             (zip formal actual)
          ftype' = foldr (uncurry substituteTypeVar) ftype substitutions
          unresolvedVars = filter
                             (not . (flip elem) (fst <$> substitutions))
                             tlist
      errorWhen (length unresolvedVars > 0)
                (TypeKindError ftype' "function without type variables")
      makeFunctionCallAnnotation ftype' actual
makeFunctionCallAnnotation ftype actual =
    throwError $ TypeMismatch ftype (FunctionType actual InferableType)
                              "not a function"


resolveStatement :: Scope -> Statement -> KStat s AStatement
resolveStatement s (Expression expr) = do
    aexpr <- resolveExpr s expr
    return $ AExpression (StmtAnnotation (Just (aexprType aexpr)) [] []) aexpr

resolveStatement s (VarDeclStatement name tdesc varinit) = do
    -- reuse the existing code for resolving a top-level variable definition
    (VariableDefinition rtdesc rvarinit) <-
        resolveDefinition s (VariableDefinition tdesc varinit)
    return $ AVarDeclStatement
              (StmtAnnotation Nothing
                              [(name,VariableDefinition rtdesc VarInitNone)] [])
              name rtdesc rvarinit

resolveStatement s (StatementBlock ss) = do
    -- it's annoying that this produces a reversed block, but unfortunately
    -- we *must* use a left fold in order to have results of previous
    -- statement resolution available, and right folds cannot efficiently
    -- produce non-reversed lists.
    (s', reversedBlock) <- foldM resolvePrepend (s, []) ss
    return $ AStatementBlock
               (makeAnnotation reversedBlock)
               (reverse reversedBlock)
    where
      resolvePrepend :: (Scope, [AStatement]) -> Statement ->
                        KStat s (Scope, [AStatement])
      resolvePrepend (cs, reversedBlock) stmt = do
          astmt <- resolveStatement cs stmt
          return (updatedScope cs (astmtAnnotation astmt), astmt:reversedBlock)

      makeAnnotation (stmt:_) = StmtAnnotation (astmtType stmt) [] []
      makeAnnotation []       = StmtAnnotation Nothing [] []

      updatedScope cs (StmtAnnotation _ [] _) = cs
      updatedScope cs (StmtAnnotation _ dl _) = foldl' (|@+|) cs dl

resolveInstance :: Scope -> FunctionInstance -> KStat s FunctionInstance
resolveInstance _ afi@(AFunctionInstance _ _ _) = return afi
resolveInstance s (FunctionInstance td params st) = do
    atd <- typeLookup s td
    ast <- resolveStatement (makeFunctionScope s td params) st
    astType <- errorIfNothing (astmtType ast)  -- fixme void functions?
                 (TypeKindError (functionTypeReturn td)
                                ErrorMessages.noReturn)
    errorWhenNot ((functionTypeReturn atd) `typeCompatible` astType)
                 (TypeMismatch astType (functionTypeReturn atd)
                               ErrorMessages.incompatibleReturn)
    return $ AFunctionInstance
                atd           -- fixme what about return type specialization?
                params        -- instance still accepts the same params
                ast           -- body is fully resolved

-- | resolve a function instance's definition, but do not inspect its actual
-- implementation (which cannot be done safely until after all of a module's
-- type definitions are resolved).
resolveInstanceTypes :: Scope -> FunctionInstance -> KStat s FunctionInstance
resolveInstanceTypes _ afi@(AFunctionInstance _ _ _) = return afi
resolveInstanceTypes s (FunctionInstance td params st) = do
    atd <- typeLookup s td
    return $ FunctionInstance
                atd
                params
                st
