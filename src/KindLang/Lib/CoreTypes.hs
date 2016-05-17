module KindLang.Lib.CoreTypes where

import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.Scope
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.KStat
import qualified Data.Map as Map

-- | Function to create an ID in the core module given a string
coreId :: String -> NSID
coreId i =  (UnqualifiedID i) `qualifiedBy` sidKind

coreTypes :: Catalogue
coreTypes = namespaceCatalogue $
             expectNoErrors "kind::* undefined" (coreTypesQualified |@| sidKind)

coreTypesQualified :: Catalogue
coreTypesQualified =
    newCatalogue |+| (sidKindInt, InternalTypeDefinition)
                 |+| (sidKindString, InternalTypeDefinition)
                 |+| (coreId "(+)", InternalObject fnIntIntInt)

scopeDefault :: Scope
scopeDefault = Scope Nothing coreTypes

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
rtKindInt = expectNoErrors "Internal error: int not defined" $
            resolveType coreTypes sidInt
eaKindInt :: ExprAnnotation
eaKindInt = ExprAnnotation rtKindInt []
saKindInt :: StmtAnnotation
saKindInt = StmtAnnotation (Just rtKindInt) [] []
sarefKindInt :: StmtAnnotation
sarefKindInt = StmtAnnotation (Just $ Reference rtKindInt) [] []

rtKindString :: TypeDescriptor
rtKindString = expectNoErrors "Internal error: string not defined" $
               resolveType coreTypes sidString
eaKindString :: ExprAnnotation
eaKindString = ExprAnnotation rtKindString []
saKindString :: StmtAnnotation
saKindString = StmtAnnotation (Just rtKindString) [] []
sarefKindString :: StmtAnnotation
sarefKindString = StmtAnnotation (Just $ Reference rtKindString) [] []

fnIntIntInt :: TypeDescriptor
fnIntIntInt = FunctionType [rtKindInt,rtKindInt] rtKindInt
fnIntInt :: TypeDescriptor
fnIntInt = FunctionType [rtKindInt] rtKindInt
