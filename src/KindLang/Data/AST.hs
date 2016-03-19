module KindLang.Data.AST where

data ScopedID = 
     UnqualifiedID String |
     QualifiedID String ScopedID
     deriving (Show, Eq, Ord)
    
data ModuleImport = 
     UnqualifiedModuleImport ScopedID Bool |
     QualifiedModuleImport ScopedID Bool ScopedID
     deriving (Show, Eq)
     
data Module = Module {
     moduleName :: Maybe ScopedID,
     moduleImportList :: [ModuleImport],
     moduleDeclarationList :: [(String,Definition)]
} deriving (Show, Eq)

data Definition =
     ClassDefinition [ClassMember] |
     FunctionDefinition [(String,TypeDescriptor)] TypeDescriptor [Expr] |
     VariableDefinition TypeDescriptor VariableInitializer
     deriving (Show, Eq)

data ClassMember = ClassMember String Visibility Definition
     deriving (Show, Eq)

data Visibility = Public | Protected | Private
     deriving (Show, Eq)
              
data TypeDescriptor = SimpleType ScopedID | InferableType
     deriving (Show, Eq)

data Expr = VarRef String
     deriving (Show, Eq)

data VariableInitializer =
     VarInitNone |
     VarInitExpr Expr |
     VarInitConstruct [Expr]
     deriving (Show, Eq)
