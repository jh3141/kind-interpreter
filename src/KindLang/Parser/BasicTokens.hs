module KindLang.Parser.BasicTokens where

import Text.Parsec
import KindLang.Parser.Combinators
import KindLang.Parser.State
import Control.Monad
import KindLang.Util.Control
import KindLang.Data.AST
    
-- common separator characters
semicolon :: Parser Char
semicolon = char ';'
colon :: Parser Char
colon = char ':'
comma :: Parser Char
comma = char ','

identifier_ :: Parser String
identifier_ = startChar <:> many continueChar 
    where
        startChar = letter <|> oneOf "_~"
        continueChar = startChar <|> digit

scopeOp_ :: Parser String
scopeOp_ = string "::"
            
scopedID_ :: Parser ScopedID
scopedID_ = liftM (foldrn QualifiedID UnqualifiedID) $
                   sepBy1Lazy (withtws identifier_) (withtws scopeOp_)

operator_ :: Parser String
operator_  = (many1 $ oneOf "!$%^&*-+=~#<>:@\\|") <|>
             string "(" <|>
             string "," <|>
             string "."

intLiteral_ :: Parser Expr
intLiteral_ = many1 digit >>= return . IntLiteral . read 
