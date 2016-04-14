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
resolveDefListTypes :: Catalogue -> DefList -> DefList -- FIXME errors?
resolveDefListTypes cat = map (second $ resolveDefinition cat)

-- | Return a copy of a class member definition list resolved against a
-- given catalogue.
resolveClassDefListTypes :: Catalogue -> [ClassMember] -> [ClassMember] -- FIXME errors?
resolveClassDefListTypes cat = map (resolveClassMember cat)
resolveClassMember :: Catalogue -> ClassMember -> ClassMember
resolveClassMember cat (ClassMember n v d) = ClassMember n v
                                             (resolveDefinition cat d)

-- | Resolve an individual definition against a given catalogue.
resolveDefinition :: Catalogue -> Definition -> Definition -- FIXME errors?
resolveDefinition cat (VariableDefinition (SimpleType sid) i) =
    case lookupHierarchical cat sid of -- fixme how do we use resolveType here?
      Left err -> error $ show err -- FIXME
      Right (cid, def) -> VariableDefinition (ResolvedType sid cid def) i
resolveDefinition cat (ClassDefinition cdlist) =
    ClassDefinition $ resolveClassDefListTypes cat cdlist                   
resolveDefinition _ nonMatching = nonMatching

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
-- function application
resolveExpr cat (FunctionApplication fnExpr paramExprs) = do
    rFnExpr <- resolveExpr cat fnExpr
    rParamExprs <- sequence $ fmap (resolveExpr cat) paramExprs
    annotation <- makeFunctionCallAnnotation
                    (aexprType rFnExpr)
                    (aexprType <$> rParamExprs)
    return $ AFunctionApplication annotation rFnExpr rParamExprs

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
