module KindLang.Parser.StatementParser where

import Text.Parsec
    
import KindLang.Data.AST
import KindLang.Parser.State
import KindLang.Parser.Combinators
import KindLang.Parser.BasicTokens
import KindLang.Parser.ExpressionParser (expr_)

type StatementP = Parser Statement

stmt_ :: StatementP
stmt_ = (Expression <$> withtws expr_ <* semicolon)
