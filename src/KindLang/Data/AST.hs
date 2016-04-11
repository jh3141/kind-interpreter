module KindLang.Data.AST where

import Data.Maybe
import KindLang.Data.BasicTypes

type DefList = [(String,Definition)]
    
data ModuleImport = 
     UnqualifiedModuleImport ScopedID Bool |
     QualifiedModuleImport ScopedID Bool (Maybe ScopedID)
     deriving (Show, Eq)
     
data Module = Module {
     moduleName :: Maybe ScopedID,
     moduleImportList :: [ModuleImport],
     moduleDeclarationList :: DefList     -- FIXME shouldn't this be called moduleDefinitionList?
} deriving (Show, Eq)

data FunctionInstance = FunctionInstance {
         fnDefParams :: [(String,TypeDescriptor)],
         fnDefReturnType :: TypeDescriptor,
         fnDefBody :: [Statement]
     }
     deriving (Show, Eq)
              
data Definition =
     ClassDefinition [ClassMember] |
     FunctionDefinition [FunctionInstance] |
     VariableDefinition TypeDescriptor VariableInitializer |
     Namespace (IdentMap Definition)
     deriving (Show, Eq)
type IdentDefinition = Identified Definition
    
data ClassMember = ClassMember String Visibility Definition
     deriving (Show, Eq)

data Visibility = Public | Protected | Private
     deriving (Show, Eq)
              
data TypeDescriptor =
     SimpleType ScopedID |
     InferableType |
     ResolvedType {
         resolvedTypeRID :: ScopedID,
         resolvedTypeCanonicalID :: ScopedID,
         resolvedTypeDefinition :: Definition
     }
     deriving (Show, Eq)

data Expr =
     Annotated AExpr |
     VarRef ScopedID |
     IntLiteral Int |
     StringLiteral String |
     BinOp String Expr Expr |
     PrefixOp String Expr |
     FunctionApplication Expr [Expr] |
     ORef Expr ScopedID |
     OMethod Expr ScopedID [Expr]
     deriving (Show, Eq)

data AExpr =
     AVarRef ExprAnnotation ScopedID |
     AIntLiteral ExprAnnotation Int |
     AStringLiteral ExprAnnotation String |
     ABinOp ExprAnnotation String AExpr AExpr |
     APrefixOp ExprAnnotation String AExpr |
     AFunctionApplication ExprAnnotation AExpr [AExpr] |
     AORef ExprAnnotation AExpr ScopedID |
     AOMethod ExprAnnotation AExpr ScopedID [AExpr]
     deriving (Show, Eq)

data ExprAnnotation =
     ExprAnnotation TypeDescriptor [(String,ExprAnnotationData)]
     deriving (Show, Eq)

data ExprAnnotationData =
     EADOptionTrue |
     EADId ScopedID
     deriving (Show, Eq)
              
data Statement =
     Expression Expr |
     VarDeclStatement String TypeDescriptor VariableInitializer
     deriving (Show, Eq)
              
data VariableInitializer =
     VarInitNone |
     VarInitExpr Expr |
     VarInitConstruct [Expr]
     deriving (Show, Eq)


-- functions for common manipulations of the AST
maybeOrInferable :: Maybe TypeDescriptor -> TypeDescriptor
maybeOrInferable = fromMaybe InferableType

fnDefInstances :: Definition -> [FunctionInstance]
fnDefInstances (FunctionDefinition i) = i
fnDefInstances _ = []
                   
                                 
definitionTypeName :: Definition -> String
definitionTypeName (ClassDefinition _) = "class"
definitionTypeName (FunctionDefinition _) = "function"
definitionTypeName (VariableDefinition _ _) = "variable"
definitionTypeName (Namespace _) = "namespace"
                                   
