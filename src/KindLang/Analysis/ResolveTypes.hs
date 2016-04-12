module KindLang.Analysis.ResolveTypes where

import Data.List
import Control.Arrow
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Catalogue
import KindLang.Data.Error
import KindLang.Lib.CoreTypes    
 
resolveTypes :: Module -> Module
resolveTypes m = m
                   
resolveDefListTypes :: Catalogue -> DefList -> DefList
resolveDefListTypes cat = map (second $ resolveDefinition cat)

resolveClassDefListTypes :: Catalogue -> [ClassMember] -> [ClassMember]
resolveClassDefListTypes cat = map (resolveClassMember cat)
resolveClassMember :: Catalogue -> ClassMember -> ClassMember
resolveClassMember cat (ClassMember n v d) = ClassMember n v
                                             (resolveDefinition cat d)

resolveDefinition :: Catalogue -> Definition -> Definition
resolveDefinition cat (VariableDefinition (SimpleType sid) i) =
    case lookupHierarchical cat sid of -- fixme how do we use resolveType here?
      Left err -> error $ show err -- FIXME
      Right (cid, def) -> VariableDefinition (ResolvedType sid cid def) i
                          
resolveDefinition cat (ClassDefinition cdlist) =
    ClassDefinition $ resolveClassDefListTypes cat cdlist
                    
resolveDefinition _ nonMatching = nonMatching

resolveExpr :: Catalogue -> Expr -> KErr AExpr
-- pre-annotated expressions can simply be returned                
resolveExpr _ (Annotated aexpr) = Right aexpr
-- literals have predefined types                                  
resolveExpr cat (IntLiteral v) = Right $ AIntLiteral eaKindInt v
resolveExpr cat (StringLiteral v) = Right $ AStringLiteral eaKindString v
                                    
resolveExpr cat (VarRef sid) =
    case makeAnnotationForDefinition of
      Left err -> Left err
      Right ann -> Right $ AVarRef ann sid
    where
      makeAnnotationForDefinition :: KErr ExprAnnotation
      makeAnnotationForDefinition =
          lookupHierarchical cat sid >>= identDefToExprAnnotation

resolveExpr cat (ORef oexpr sid) = do
    aoexpr <- resolveExpr cat oexpr
    (cid, refType) <- resolveTypeRef cat (aexprType aoexpr) sid
    return $ AORef (ExprAnnotation refType [("CanonicalID", EADId cid)])
                   aoexpr sid
    
                             
identDefToExprAnnotation :: IdentDefinition -> KErr ExprAnnotation
identDefToExprAnnotation (cid, VariableDefinition rt@(ResolvedType _ _ _) _) =
    Right $ ExprAnnotation rt [("CanonicalID", EADId cid)]
identDefToExprAnnotation (cid, VariableDefinition rt _) =
    Left $ InternalError (show cid ++ " is not resolved (" ++ show rt ++")")
identDefToExprAnnotation (cid, def) =
    Left $ TypeError cid ("referenced as a variable but is a " ++
                          (definitionTypeName def))
                               

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
    Left $ InternalError ("dereferenced object is not resolved (" ++ show t ++ ")")
         
