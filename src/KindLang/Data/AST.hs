module KindLang.Data.AST where

data Module = Module {
    moduleName :: String,
    moduleImportList :: [String],
    moduleDeclarationList :: [(String,Definition)]
} deriving (Show)

data Definition =
    ClassDefinition |
    FunctionDefinition
    deriving (Show)
