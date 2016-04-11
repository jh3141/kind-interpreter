module KindLang.Analysis.ResolveTypes where

import Control.Arrow
import KindLang.Data.AST
import KindLang.Data.Catalogue
    
 
resolveTypes :: Module -> Module
resolveTypes mod = mod
                   
resolveDeflistTypes :: Catalogue -> DefList -> DefList
resolveDeflistTypes cat = map (second $ resolveDefinition cat)

resolveDefinition :: Catalogue -> Definition -> Definition
resolveDefinition cat (VariableDefinition (SimpleType sid) i) =
    case lookupHierarchical cat sid of
      Left err -> error $ show err -- FIXME
      Right (cid, def) -> VariableDefinition (ResolvedType sid cid def) i
resolveDefinition cat nonMatching = nonMatching
