module KindLang.Analysis.ResolveTypes where

import Control.Arrow
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Catalogue
import KindLang.Data.Error
    
 
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
    case lookupHierarchical cat sid of
      Left err -> error $ show err -- FIXME
      Right (cid, def) -> VariableDefinition (ResolvedType sid cid def) i
                          
resolveDefinition cat (ClassDefinition cdlist) =
    ClassDefinition $ resolveClassDefListTypes cat cdlist
                    
resolveDefinition _ nonMatching = nonMatching

                                    
resolveExpr :: Catalogue -> Expr -> KErr AExpr
resolveExpr cat (VarRef sid) =
    case makeAnnotationForDefinition of
      Left err -> Left err
      Right ann -> Right $ AVarRef ann sid
    where
      makeAnnotationForDefinition :: KErr ExprAnnotation
      makeAnnotationForDefinition =
          case lookupHierarchical cat sid of
            Left err -> Left err
            Right (cid, VariableDefinition rt@(ResolvedType _ _ _) _) ->
                Right $ ExprAnnotation rt [("CanonicalID", EADId cid)]
            Right (cid, VariableDefinition rt _) ->
                Left $ InternalError
                         (show cid ++ " is not resolved (" ++ show rt ++")")
            Right (cid, def) ->
                Left $ TypeError cid ("referenced as a variable but is a " ++
                         (definitionTypeName def))
                               
