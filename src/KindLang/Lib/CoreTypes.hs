module KindLang.Lib.CoreTypes where

import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST
import KindLang.Data.KStat
import KindLang.Data.Scope((|++|),resolveTypeKS)
import KindLang.Runtime.Data

-- | Function to create an ID in the core module given a string
coreId :: String -> NSID
coreId i =  (UnqualifiedID i) `qualifiedBy` sidKind

scopeDefault :: KStat s (Scope s)
scopeDefault =
    (Scope Nothing <$> newCatalogue)
           |++| (sidInt, sidKindInt, InternalTypeDefinition)
           |++| (sidString, sidKindString, InternalTypeDefinition)

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
            resolveTypeKS scopeDefault sidInt
eaKindInt :: ExprAnnotation
eaKindInt = ExprAnnotation rtKindInt []
saKindInt :: StmtAnnotation
saKindInt = StmtAnnotation (Just rtKindInt) [] []
sarefKindInt :: StmtAnnotation
sarefKindInt = StmtAnnotation (Just $ Reference rtKindInt) [] []

rtKindString :: TypeDescriptor
rtKindString = expectNoErrors "Internal error: string not defined" $
               resolveTypeKS scopeDefault sidString
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
