{-# LANGUAGE TupleSections #-}
module KindLang.Analysis.ResolveTypes where

import Data.List
import Control.Arrow
import Control.Monad
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Catalogue
import KindLang.Data.Error
import KindLang.Data.Scope
import KindLang.Lib.CoreTypes    

-- | Return a copy of a module tree with all types resolved, or an error message.
resolveTypes :: Module -> KErr Module
resolveTypes m = Right m -- FIXME implement this

-- | Return a copy of a definition list resolved against a given scope.
resolveDefListTypes :: Scope -> DefList -> KErr DefList
resolveDefListTypes s =
    mapM resolve
    where
      resolve :: (String,Definition) -> KErr (String,Definition)
      resolve (cid,def) = (cid,) <$> resolveDefinition s def

-- | Return a copy of a class member definition list resolved against a
-- given scope.
resolveClassDefListTypes :: Scope -> [ClassMember] -> KErr [ClassMember]
resolveClassDefListTypes s = mapM (resolveClassMember s)

-- | Resolve an individual class member definition against a given scope.
resolveClassMember :: Scope -> ClassMember -> KErr ClassMember
resolveClassMember s (ClassMember n v d) =
    ClassMember n v <$> resolveDefinition s d

-- | Resolve an individual definition against a given scope.
resolveDefinition :: Scope -> Definition -> KErr Definition 
resolveDefinition s (VariableDefinition (SimpleType sid) i) = do
    -- fixme how do we use resolveType here?
    -- fixme if 'i' is an init expression it should be resolved & typechecked.
    -- fixme if 'i' is a constructor expression it should be resolved.
    (cid, def) <- scopeLookup s sid
    return $ VariableDefinition (ResolvedType sid cid def) i
resolveDefinition s (VariableDefinition InferableType (VarInitExpr e)) = do
    ae <- resolveExpr s e
    return $ VariableDefinition (aexprType ae) (VarInitAExpr ae)
resolveDefinition s (ClassDefinition cdlist) =
    ClassDefinition <$> resolveClassDefListTypes s cdlist
resolveDefinition _ nonMatching = Right nonMatching

-- | Resolve an expression tree against a given scope, returning a resolved
-- copy (an 'AExpr' with the same meaning as the unresolved 'Expr') or an error.
resolveExpr :: Scope -> Expr -> KErr AExpr
-- pre-annotated expressions can simply be returned                
resolveExpr _ (Annotated aexpr) = Right aexpr
-- literals have predefined types                                  
resolveExpr _ (IntLiteral v) = Right $ AIntLiteral eaKindInt v
resolveExpr _ (StringLiteral v) = Right $ AStringLiteral eaKindString v
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
identDefToExprAnnotation :: IdentDefinition -> KErr ExprAnnotation
identDefToExprAnnotation (cid, VariableDefinition rt@(ResolvedType _ _ _) _) =
    Right $ crefAnnotation cid rt
identDefToExprAnnotation (cid, VariableDefinition rt _) =
    Left $ InternalError (nsidString cid ++ " is not resolved (" ++
                                         show rt ++")")
identDefToExprAnnotation (cid, FunctionDefinition []) =
    Left $ InternalError (nsidString cid ++ " contains no instances")
identDefToExprAnnotation (cid, FunctionDefinition (fnInstance:[])) =
    Right $ crefAnnotation cid (fnInstanceType fnInstance)
identDefToExprAnnotation (cid, FunctionDefinition _) =
    error "overloaded functions not implemented"
identDefToExprAnnotation (cid, def) =
    Left $ TypeError cid ("referenced as a variable but is a " ++
                          (definitionTypeName def))

-- | Utility function to build a type descriptor for a given function instance.
fnInstanceType :: FunctionInstance -> TypeDescriptor
fnInstanceType (FunctionInstance params rtype _) =
    FunctionType (snd <$> params) rtype

-- | @resolveTypeRef s desc sid@ returns the canonical identifier and type
-- descriptor of an item whose identifier is @sid@ residing inside an object
-- of type @desc@ refered to in scope @s@, or an error message if no such
-- object can be resolved.
resolveTypeRef :: Scope -> TypeDescriptor -> NSID ->
                  KErr (Identified TypeDescriptor)
resolveTypeRef _ (ResolvedType _ cid (ClassDefinition members))
                 sid@(UnqualifiedID memberId) = 
    -- fixme may be better if classdef has a map rather than a list?
    case find ((== memberId) . classMemberName) members of
      Nothing -> Left $ IdentifierNotFound $ fqid
      Just (ClassMember _ Public def) -> do
          (ExprAnnotation rt _) <- identDefToExprAnnotation (fqid, def)
          return (fqid, rt)
      Just (ClassMember _ access _) -> Left $ AccessViolation fqid access
    where
      fqid = sid `qualifiedBy` cid
    -- fixme what to do with qualified member references?
resolveTypeRef _ (ResolvedType _ cid def) sid =
    Left $ TypeError (sid `qualifiedBy` cid)
             ((nsidString sid) ++ " is a " ++ (definitionTypeName def))
resolveTypeRef _ t _ =
    Left $ InternalError ("dereferenced object is not resolved (" ++
                          show t ++ ")")

-- fixme how should overloaded functions work?
-- | Given a function type and the types of arguments passed to it, build
-- an oppropriate annotation for the result of applying the arguments to the
-- function, or an error message if the application is malformed.
makeFunctionCallAnnotation :: TypeDescriptor -> [TypeDescriptor] ->
                              KErr ExprAnnotation
makeFunctionCallAnnotation (FunctionType formal res) actual
    | typesCompatible formal actual = Right $ ExprAnnotation res []
    | otherwise                     = Left  $ InvalidApplication formal actual
makeFunctionCallAnnotation ftype _  = Left  $ TypeMismatch ftype "function"

-- | @typesCompatible f a@ determines whether a function whose formal parameters
-- have types @f@ can be invoked with actual parameters of type @a@, returning
-- True or False.
typesCompatible :: [TypeDescriptor] -> [TypeDescriptor] -> Bool
typesCompatible = (==)   -- fixme - subtypes?

-- fixme this should be in a file by itself, not stuck down here.
-- fixme it should also work, rather than hack a result for the tests!
findBinaryOperator :: String -> TypeDescriptor -> TypeDescriptor -> KErr AExpr
findBinaryOperator "+" t1 t2 =
    Right $ AInternalRef
              (ExprAnnotation (FunctionType [t1, t2] t2) [])
              (coreId "(+)")
findBinaryOperator _ _ _ =
    Left $ InternalError "haven't finished implementing operators"
-- ditto
findPrefixOperator :: String -> TypeDescriptor -> KErr AExpr
findPrefixOperator "-" t =
    Right $ AInternalRef (ExprAnnotation (FunctionType [t] t) []) (coreId "(u-)")
findPrefixOperator _ _ =
    Left $ InternalError "haven't finished implementing operators"

resolveStatement :: Scope -> Statement -> KErr AStatement
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
    -- statement resolution available, and right folds cannot efficiently produce
    -- non-reversed lists.
    (s', reversedBlock) <- foldM resolvePrepend (s, []) ss
    return $ AStatementBlock
               (makeAnnotation reversedBlock)
               (reverse reversedBlock)
    where
      resolvePrepend :: (Scope, [AStatement]) -> Statement -> 
                        KErr (Scope, [AStatement])
      resolvePrepend (cs, reversedBlock) stmt = do
          astmt <- resolveStatement cs stmt
          return (updatedScope cs (astmtAnnotation astmt), astmt:reversedBlock)
                 
      makeAnnotation (stmt:_) = StmtAnnotation (astmtType stmt) [] []
      makeAnnotation []       = StmtAnnotation Nothing [] []

      updatedScope cs (StmtAnnotation _ [] _) = cs
      updatedScope cs (StmtAnnotation _ dl _) = foldl' (|@+|) cs dl

resolveInstance :: Scope -> FunctionInstance -> KErr FunctionInstance
resolveInstance s fn = Right fn
