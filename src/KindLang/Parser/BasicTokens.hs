module KindLang.Parser.BasicTokens where

import Text.Parsec
import KindLang.Parser.Combinators
import Control.Monad
import KindLang.Util.Control
import KindLang.Data.AST
    
-- common separator characters
semicolon :: Parsec String u Char
semicolon = char ';'
colon :: Parsec String u Char
colon = char ':'
comma :: Parsec String u Char
comma = char ','

identifier_ :: Parsec String u String
identifier_ = startChar <:> many continueChar 
    where
        startChar = letter <|> oneOf "_~"
        continueChar = startChar <|> digit

scopeOp_ :: Parsec String u String
scopeOp_ = string "::"
            
scopedID_ :: Parsec String u ScopedID
scopedID_ = liftM (foldrn QualifiedID UnqualifiedID) $
                   sepBy1Lazy (withtws identifier_) (withtws scopeOp_)

