module KindLang.Analysis.ResolveTypes where

import Control.Arrow
import KindLang.Data.AST
import KindLang.Data.Catalogue
    
 
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

                                    
