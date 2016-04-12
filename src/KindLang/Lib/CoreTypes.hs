module KindLang.Lib.CoreTypes where

import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST
    
coreTypes :: Catalogue
coreTypes =
    newCatalogue |++| (sidInt, sidKindInt, InternalTypeDefinition)
                 |++| (sidString, sidKindString, InternalTypeDefinition)

coreTypesQualified :: Catalogue
coreTypesQualified =
    newCatalogue |+| (sidKind, Namespace coreTypes)
        
-- note convention of naming of kind types:
--   namespaces - lower case
--   value types - lower case
--   reference types - upper case

sidKind :: ScopedID
sidKind = UnqualifiedID "kind"
sidInt :: ScopedID
sidInt = UnqualifiedID "int"
sidString :: ScopedID
sidString = UnqualifiedID "string"

sidKindInt :: ScopedID
sidKindInt = sidInt `qualifiedBy` sidKind
sidKindString :: ScopedID
sidKindString = sidString `qualifiedBy` sidKind

-- fixme should these be qualified or unqualified?
rtKindInt :: TypeDescriptor
rtKindInt = either (error "Internal error: int not defined") id $
            resolveType coreTypes sidInt
eaKindInt :: ExprAnnotation
eaKindInt = ExprAnnotation rtKindInt []
            
rtKindString :: TypeDescriptor
rtKindString = either (error "Internal error: string not defined") id $
               resolveType coreTypes sidString
eaKindString :: ExprAnnotation
eaKindString = ExprAnnotation rtKindString []
               
