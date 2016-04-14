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
     Namespace (IdentMap Definition) |
     InternalTypeDefinition | -- fixme make this go away
     InternalObject TypeDescriptor {- fixme add execution stub -}
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
     } |
     FunctionType [TypeDescriptor] TypeDescriptor
     deriving (Show, Eq)

data Expr =
     Annotated AExpr |
     IntLiteral Int |
     StringLiteral String |
     VarRef ScopedID |
     ORef Expr ScopedID |
     BinOp String Expr Expr |
     PrefixOp String Expr |
     FunctionApplication Expr [Expr] |
     OMethod Expr ScopedID [Expr]
     deriving (Show, Eq)

data AExpr =
     AIntLiteral ExprAnnotation Int |
     AStringLiteral ExprAnnotation String |
     AVarRef ExprAnnotation ScopedID |
     AORef ExprAnnotation AExpr ScopedID |
     -- operators are transformed to function/method applications during
     -- type annotation so do not appear here.
     AFunctionApplication ExprAnnotation AExpr [AExpr] |
     AOMethod ExprAnnotation AExpr ScopedID [AExpr] |
     -- internal references generated during resolution, e.g. internal functions
     AInternalRef ExprAnnotation ScopedID
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
definitionTypeName (InternalTypeDefinition) = "internal-type"
definitionTypeName (InternalObject _) = "internal-object"
                                      
aexprAnnotation :: AExpr -> ExprAnnotation
aexprAnnotation (AIntLiteral a _) = a
aexprAnnotation (AStringLiteral a _) = a
aexprAnnotation (AVarRef a _) = a
aexprAnnotation (AORef a _ _) = a                      
aexprAnnotation (AFunctionApplication a _ _) = a
aexprAnnotation (AOMethod a _ _ _) = a
aexprAnnotation (AInternalRef a _) = a
                                     
exprAnnotationType :: ExprAnnotation -> TypeDescriptor
exprAnnotationType (ExprAnnotation t _) = t
                                                
aexprType :: AExpr -> TypeDescriptor
aexprType = exprAnnotationType . aexprAnnotation


classMemberName :: ClassMember -> String
classMemberName (ClassMember name _ _) = name

namespaceCatalogue :: Definition -> IdentMap Definition
namespaceCatalogue (Namespace cat) = cat
namespaceCatalogue t = error ("not a namespace: " ++ definitionTypeName t)
