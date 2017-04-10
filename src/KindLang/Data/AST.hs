module KindLang.Data.AST where

import Data.Maybe
import KindLang.Data.BasicTypes

type DefList = [(String,Definition)]
    
data ModuleImport = 
     UnqualifiedModuleImport NSID Bool |
     QualifiedModuleImport NSID Bool (Maybe NSID)
     deriving (Show, Eq)
     
data Module = Module {
     moduleName :: Maybe NSID,
     moduleImportList :: [ModuleImport],
     moduleDeclarationList :: DefList     -- FIXME shouldn't this be called moduleDefinitionList?
} deriving (Show, Eq)

data FunctionInstance =
     FunctionInstance TypeDescriptor [String] Statement
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
     SimpleType NSID |
     InferableType |
     FunctionType [TypeDescriptor] TypeDescriptor |
     ForAllTypes [String] [TypePredicate] TypeDescriptor |
     TypeVariable String |
     SumType [TypeDescriptor] |
     TupleType [TypeDescriptor] |
     RecordType NSID [TypeDescriptor] |
     Reference TypeDescriptor
     deriving (Show, Eq)

data TypePredicate =
     NotType TypePredicate |
     TypesEqual TypeDescriptor TypeDescriptor
     deriving (Show, Eq)
                   
data Expr =
     IntLiteral Int |
     StringLiteral String |
     VarRef NSID |
     ORef Expr NSID |
     BinOp String Expr Expr |
     PrefixOp String Expr |
     FunctionApplication Expr [Expr] |
     OMethod Expr NSID [Expr]
     deriving (Show, Eq)

data Statement =
     Expression Expr |
     VarDeclStatement String TypeDescriptor VariableInitializer |
     StatementBlock [Statement]
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

classMemberName :: ClassMember -> String
classMemberName (ClassMember name _ _) = name

namespaceCatalogue :: Definition -> IdentMap Definition
namespaceCatalogue (Namespace cat) = cat
namespaceCatalogue t = error ("not a namespace: " ++ definitionTypeName t)

-- | Convert a statement list to a single statement. Empty lists become
-- a statement block, as do lists with multiple statements, but lists with
-- just one statement are unwrapped.
statementListToStatement :: [Statement] -> Statement
statementListToStatement (s:[]) = s
statementListToStatement ss = StatementBlock ss

nullStatement :: Statement
nullStatement = StatementBlock []

fnInstanceBody :: FunctionInstance -> Statement
fnInstanceBody (FunctionInstance _ _ b) = b

fnInstanceType :: FunctionInstance -> TypeDescriptor
fnInstanceType (FunctionInstance td _ _) = td

fnInstanceArgs :: FunctionInstance -> [String]
fnInstanceArgs (FunctionInstance _ a _) = a

functionTypeReturn :: TypeDescriptor -> TypeDescriptor
functionTypeReturn (FunctionType _ r) = r
functionTypeReturn n = n -- maybe should be an error?

functionTypeArgs :: TypeDescriptor -> [TypeDescriptor]
functionTypeArgs (FunctionType r _) = r
functionTypeArgs _ = []

