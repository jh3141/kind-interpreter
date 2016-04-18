module KindLang.Lib.CoreTypes where

import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST
import KindLang.Util.Control
import qualified Data.Map as Map

-- | Function to create an ID in the core module given a string
coreId :: String -> NSID
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

sidKind :: NSID
sidKind = UnqualifiedID "kind"
sidInt :: NSID
sidInt = UnqualifiedID "int"
sidString :: NSID
sidString = UnqualifiedID "string"

sidKindInt :: NSID
sidKindInt = sidInt `qualifiedBy` sidKind
sidKindString :: NSID
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
fnIntInt :: TypeDescriptor
fnIntInt = FunctionType [rtKindInt] rtKindInt
              
