{-# LANGUAGE TupleSections #-}
module KindLang.Analysis.ResolveTypes where

import Data.List
import Control.Arrow
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Catalogue
import KindLang.Data.Error
import KindLang.Lib.CoreTypes    

-- | Return a copy of a module tree with all types resolved, or an error message.
resolveTypes :: Module -> KErr Module
resolveTypes m = Right m -- FIXME implement this

-- | Return a copy of a definition list resolved against a given catalogue.
resolveDefListTypes :: Catalogue -> DefList -> KErr DefList
resolveDefListTypes cat =
    mapM resolve
    where
      resolve :: (String,Definition) -> KErr (String,Definition)
      resolve (cid,def) = (cid,) <$> resolveDefinition cat def

-- | Return a copy of a class member definition list resolved against a
-- given catalogue.
resolveClassDefListTypes :: Catalogue -> [ClassMember] -> KErr [ClassMember]
resolveClassDefListTypes cat = mapM (resolveClassMember cat)

-- | Resolve an individual class member definition against a given catalogue.
resolveClassMember :: Catalogue -> ClassMember -> KErr ClassMember
resolveClassMember cat (ClassMember n v d) =
    ClassMember n v <$> resolveDefinition cat d

-- | Resolve an individual definition against a given catalogue.
resolveDefinition :: Catalogue -> Definition -> KErr Definition 
resolveDefinition cat (VariableDefinition (SimpleType sid) i) = do
    -- fixme how do we use resolveType here?
    -- fixme if 'i' is an init expression it should be resolved & typechecked.
    -- fixme if 'i' is a constructor expression it should be resolved.
    (cid, def) <- lookupHierarchical cat sid
    return $ VariableDefinition (ResolvedType sid cid def) i
resolveDefinition cat (VariableDefinition InferableType (VarInitExpr e)) = do
    ae <- resolveExpr cat e
    return $ VariableDefinition (aexprType ae) (VarInitAExpr ae)
resolveDefinition cat (ClassDefinition cdlist) =
    ClassDefinition <$> resolveClassDefListTypes cat cdlist
resolveDefinition _ nonMatching = Right nonMatching

-- | Resolve an expression tree against a given catalogue, returning a resolved
-- copy (an 'AExpr' with the same meaning as the unresolved 'Expr') or an error.
resolveExpr :: Catalogue -> Expr -> KErr AExpr
-- pre-annotated expressions can simply be returned                
resolveExpr _ (Annotated aexpr) = Right aexpr
-- literals have predefined types                                  
resolveExpr cat (IntLiteral v) = Right $ AIntLiteral eaKindInt v
resolveExpr cat (StringLiteral v) = Right $ AStringLiteral eaKindString v
-- variable and object references                                    
resolveExpr cat (VarRef sid) =
    lookupHierarchical cat sid >>=
      identDefToExprAnnotation >>=
        \ ann -> return $ AVarRef ann sid
resolveExpr cat (ORef oexpr sid) = do
    aoexpr <- resolveExpr cat oexpr
    (cid, refType) <- resolveTypeRef cat (aexprType aoexpr) sid
    return $ AORef (crefAnnotation cid refType) aoexpr sid
-- operators
resolveExpr cat (BinOp operator l r) = do
    al <- resolveExpr cat l
    ar <- resolveExpr cat r
    operatorDef <- findBinaryOperator operator (aexprType al) (aexprType ar)
    annotation <- makeFunctionCallAnnotation
                    (aexprType operatorDef)
                    ([(aexprType al), (aexprType ar)])
    return $ AFunctionApplication annotation operatorDef [al, ar]
resolveExpr cat (PrefixOp operator e) = do
    ae <- resolveExpr cat e
    operatorDef <- findPrefixOperator operator (aexprType ae)
    annotation <- makeFunctionCallAnnotation
                    (aexprType operatorDef)
                    [(aexprType ae)]
    return $ AFunctionApplication annotation operatorDef [ae]
-- function application
resolveExpr cat (FunctionApplication fnExpr paramExprs) = do
    rFnExpr <- resolveExpr cat fnExpr
    rParamExprs <- sequence $ fmap (resolveExpr cat) paramExprs
    annotation <- makeFunctionCallAnnotation
                    (aexprType rFnExpr)
                    (aexprType <$> rParamExprs)
    return $ AFunctionApplication annotation rFnExpr rParamExprs
-- object method application
resolveExpr cat (OMethod obExpr sid paramExprs) = do
    aObExpr <- resolveExpr cat obExpr
    (cid, methodType) <- resolveTypeRef cat (aexprType aObExpr) sid
    rParams <- sequence $ fmap (resolveExpr cat) paramExprs
    annotation <- makeFunctionCallAnnotation methodType (aexprType <$> rParams)
    return $ AOMethod annotation aObExpr methodType cid rParams
    
-- | Utility function to build an annotation for a reference operation           
crefAnnotation :: ScopedID -> TypeDescriptor -> ExprAnnotation
crefAnnotation cid t = (ExprAnnotation t [("CanonicalID", EADId cid)])

-- | Build an appropriate expression annotation for an expression that
-- refers to the object defined with the definition.  The definition
-- must be fully resolved and must refer to an object that has a
-- referencable value.  Returns an annotation or an error on failure.
identDefToExprAnnotation :: IdentDefinition -> KErr ExprAnnotation
identDefToExprAnnotation (cid, VariableDefinition rt@(ResolvedType _ _ _) _) =
    Right $ crefAnnotation cid rt
identDefToExprAnnotation (cid, VariableDefinition rt _) =
    Left $ InternalError (scopedIDString cid ++ " is not resolved (" ++
                                         show rt ++")")
identDefToExprAnnotation (cid, FunctionDefinition []) =
    Left $ InternalError (scopedIDString cid ++ " contains no instances")
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

-- | @resolveTypeRef cat desc sid@ returns the canonical identifier and type
-- descriptor of an item whose identifier is @sid@ residing inside an object
-- of type @desc@, or an error message if no such object can be resolved.
resolveTypeRef :: Catalogue -> TypeDescriptor -> ScopedID ->
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
             ((scopedIDString sid) ++ " is a " ++ (definitionTypeName def))
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
