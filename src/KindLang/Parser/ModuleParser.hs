module KindLang.Parser.ModuleParser where

import Text.Parsec
-- import Control.Applicative ((<*), (*>))
import Control.Monad
import Data.Maybe

import KindLang.Data.AST
import KindLang.Util.Control
import KindLang.Parser.Combinators

_module_ :: Parsec String u Module
_module_ = 
    do
        header <- optionMaybe $ withws _moduleHeader_
        body <- manyTill (withws (_import_ </> _declaration_)) eof
        case foldr updateLists ([],[]) body of
            (imports, declarations) -> return $ Module header imports declarations    
    where
        updateLists (Left a)  (listA, listB) = (a:listA, listB)
        updateLists (Right b) (listA, listB) = (listA, b:listB)

_identifier_ :: Parsec String u String
_identifier_ = startChar >>= \x -> many continueChar >>= \y -> return (x:y)
    where
        startChar = letter <|> oneOf "_~"
        continueChar = startChar <|> digit

_scopeOp_ :: Parsec String u String
_scopeOp_ = string "::"
            
_scopedID_ :: Parsec String u ScopedID
_scopedID_ = liftM (foldrn QualifiedID UnqualifiedID) $
                   sepBy1Lazy (withtws _identifier_) (withtws _scopeOp_)

_moduleHeader_ :: Parsec String u ScopedID
_moduleHeader_ = string "module" >> withlws _scopedID_ <* char ';'

-- FIXME handle more forms of import!
_import_ :: Parsec String u ModuleImport
_import_ = do
    string "import" 
    x <- withws _scopedID_
    wildcard <- optionMaybe $ string "::" >> withws (char '*')
    withtws $ char ';'
    return $ UnqualifiedModuleImport x (isJust wildcard)


type DeclarationP u = Parsec String u (String,Definition)
_declaration_ :: DeclarationP u
_declaration_ = _classDeclaration_ <|> _variableDeclaration_ -- <|> _functionDeclaration

_classDeclaration_ :: DeclarationP u
_classDeclaration_ = do
    withtws $ string "class"
    ident <- withtws _identifier_
    body <- between (char '{') (char '}') (many $ withws _declaration_)
    return (ident, ClassDefinition body)

_variableDeclaration_ :: DeclarationP u
_variableDeclaration_ = do
    ident <- withtws _identifier_
    withtws $ char ':'
    typeName <- withtws _typeDescriptor_
    constructorArgs <- optionMaybe $ withtws _functionApplication_
    char ';'
    return (ident, VariableDefinition typeName constructorArgs)

_typeDescriptor_ :: Parsec String u TypeDescriptor
_typeDescriptor_ = fmap SimpleType _scopedID_  -- or type expression

_functionApplication_ :: Parsec String u [Expr]                      
_functionApplication_ = between (withtws $ char '(') (char ')')
                                (sepBy (withtws _expr_) (withtws $ char ','))
_expr_ :: Parsec String u Expr
_expr_ = do -- FIXME!
    ident <- _identifier_
    return $ VarRef ident
