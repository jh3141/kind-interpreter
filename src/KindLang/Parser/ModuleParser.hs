module KindLang.Parser.ModuleParser where

import Text.Parsec
import Control.Applicative ((<*), (*>))
import Control.Monad

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

_whitespace_ = optional spaces
withtws p = p <* _whitespace_
withlws p = _whitespace_ *> p
withws = withtws . withlws

_identifier_ :: Parsec String u String
_identifier_ = startChar >>= \x -> many continueChar >>= \y -> return (x:y)
    where
        startChar = letter <|> oneOf "_~"
        continueChar = startChar <|> digit
        
_scopedID_ :: Parsec String u ScopedID
_scopedID_ = liftM (foldrn QualifiedID UnqualifiedID) $ sepBy1 (withtws _identifier_) (withtws $ string "::")

_moduleHeader_ :: Parsec String u ScopedID
_moduleHeader_ = string "module" >> withlws _scopedID_ <* char ';'

-- FIXME handle more forms of import!
_import_ :: Parsec String u ModuleImport
_import_ = do
    string "import" 
    x <- withws _scopedID_
    withtws $ char ';' 
    return $ UnqualifiedModuleImport x False

_declaration_ :: Parsec String u (String,Definition)
_declaration_ = _classDeclaration_ 
    where
        _classDeclaration_ = do
            withtws $ string "class"
            id <- withtws _identifier_
            body <- between (char '{') (char '}') (many $ withws _declaration_)
            return (id, ClassDefinition body)
