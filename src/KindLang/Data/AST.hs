module KindLang.Data.AST where

data ScopedID = 
    UnqualifiedID String |
    QualifiedID String ScopedID
    deriving (Show, Eq, Ord)
    
data ModuleImport = 
    UnqualifiedModuleImport ScopedID Bool |
    QualifiedModuleImport ScopedID Bool ScopedID
    deriving (Show)
     
data Module = Module {
    moduleName :: Maybe ScopedID,
    moduleImportList :: [ModuleImport],
    moduleDeclarationList :: [(String,Definition)]
} deriving (Show)

data Definition =
    ClassDefinition [(String,Definition)]|
    FunctionDefinition
    deriving (Show)
