module KindLang.Lib.CoreTypes where

import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST
import KindLang.Util.Control
import qualified Data.Map as Map

-- | Function to create an ID in the core module given a string
coreId :: String -> ScopedID
coreId i =  (UnqualifiedID i) `qualifiedBy` sidKind
          
coreTypes :: Catalogue
coreTypes = namespaceCatalogue $
              rightOrFail "kind::* undefined" (coreTypesQualified |@| sidKind)

coreTypesQualified :: Catalogue
coreTypesQualified =
    newCatalogue |+| (sidKindInt, InternalTypeDefinition)
                 |+| (sidKindString, InternalTypeDefinition)
                 |+| (coreId "(+)", InternalObject fnIntIntInt)
        
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
rtKindInt = rightOrFail "Internal error: int not defined" $
            resolveType coreTypes sidInt
eaKindInt :: ExprAnnotation
eaKindInt = ExprAnnotation rtKindInt []
            
rtKindString :: TypeDescriptor
rtKindString = rightOrFail "Internal error: string not defined" $
               resolveType coreTypes sidString
eaKindString :: ExprAnnotation
eaKindString = ExprAnnotation rtKindString []
               
fnIntIntInt :: TypeDescriptor
fnIntIntInt = FunctionType [rtKindInt,rtKindInt] rtKindInt
              
