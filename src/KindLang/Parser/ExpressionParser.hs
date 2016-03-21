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
operatorList =
    [
        OperatorInfo "*"  (LAssoc 120) binOpLed,
        OperatorInfo "/"  (LAssoc 120) binOpLed,
        OperatorInfo "%"  (LAssoc 120) binOpLed,
                     
        OperatorInfo "+"  (LAssoc 110) binOpLed,
        OperatorInfo "-"  (LAssoc 110) binOpLed,

        OperatorInfo "<<" (LAssoc 100) binOpLed,
        OperatorInfo ">>" (LAssoc 100) binOpLed,

        OperatorInfo "!=" (LAssoc  80) binOpLed,
        OperatorInfo "<"  (LAssoc  90) binOpLed,
        OperatorInfo "<=" (LAssoc  90) binOpLed,
        OperatorInfo ">"  (LAssoc  90) binOpLed,
        OperatorInfo ">=" (LAssoc  90) binOpLed,
        OperatorInfo "==" (LAssoc  80) binOpLed,

        OperatorInfo "&"  (LAssoc  70) binOpLed,
        OperatorInfo "^"  (LAssoc  60) binOpLed,                     
        OperatorInfo "|"  (LAssoc  50) binOpLed,
        OperatorInfo "&&" (LAssoc  40) binOpLed,
        OperatorInfo "^^" (LAssoc  35) binOpLed,  -- deviates from c++ version
        OperatorInfo "||" (LAssoc  30) binOpLed

    ]

binOpLed :: LeftDenotation String ParseState ParseMonad Expr String
binOpLed (OperatorInfo name prec _) lhs pp = (BinOp name lhs) <$> (pp prec)
    
prefixOperatorList :: [PrefixOperatorInfo Expr String]
prefixOperatorList = []

    
term_ :: PrecedenceParser String ParseState ParseMonad Expr -> ExprP
term_ parseSub = varRef_ <|>
                 (bracketed $ parseSub (LAssoc 0)) <|>
                 intLiteral_ 

varRef_ :: ExprP
varRef_ = fmap VarRef identifier_

    

    
    
