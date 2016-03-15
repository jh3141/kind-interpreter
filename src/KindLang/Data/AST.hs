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
     ClassDefinition[(String,Definition)]|
     FunctionDefinition [(String,TypeDescriptor)] [Expr]|
     VariableDefinition TypeDescriptor (Maybe [Expr])
     deriving (Show, Eq)

data TypeDescriptor = SimpleType ScopedID
     deriving (Show, Eq)

data Expr = VarRef String
     deriving (Show, Eq)

