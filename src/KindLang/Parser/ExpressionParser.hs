{-# LANGUAGE NoMonomorphismRestriction #-}
module KindLang.Parser.ExpressionParser where

import Text.Parsec
    
import KindLang.Data.AST
import KindLang.Parser.Combinators
import KindLang.Parser.BasicTokens
import KindLang.Parser.State
import Text.Parsec.PrattParser

type ExprP = Parser Expr
    
expr_ :: ExprP
expr_ = buildPrattParser
          operatorList prefixOperatorList whitespace_ operator_ term_

operatorList :: [OperatorInfo String ParseState ParseMonad Expr String]
operatorList = []

prefixOperatorList :: [PrefixOperatorInfo Expr String]
prefixOperatorList = []

    
term_ :: PrecedenceParser String ParseState ParseMonad Expr -> ExprP
term_ parseSub = varRef_ <|>
                 (bracketed $ parseSub (LAssoc 0)) <|>
                 intLiteral_ 

varRef_ :: ExprP
varRef_ = fmap VarRef identifier_

    

    
    
