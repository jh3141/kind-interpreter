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
        OperatorInfo "->" (RAssoc 150) reverseFunctionApplicationLed,
        OperatorInfo "("  (LAssoc 150) functionApplicationLed,
        OperatorInfo "."  (LAssoc 150) orefLed,
        OperatorInfo "\\" (LAssoc 150) binOpLed,
                     
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
        OperatorInfo "^^" (LAssoc  30) binOpLed,
        OperatorInfo "||" (LAssoc  30) binOpLed,

        OperatorInfo "="  (LAssoc  20) binOpLed,
                     
        OperatorInfo ","  (RAssoc  10) binOpLed
    ]

binOpLed :: LeftDenotation String ParseState ParseMonad Expr String
binOpLed (OperatorInfo name prec _) lhs pp = BinOp name lhs <$> pp prec

reverseFunctionApplicationLed :: LeftDenotation
                                 String ParseState ParseMonad Expr String
reverseFunctionApplicationLed (OperatorInfo _ prec _) lhs pp =
    (\ rhs -> mkFusedFunction rhs [lhs]) <$> pp prec
                                         
functionApplicationLed :: LeftDenotation
                          String ParseState ParseMonad Expr String
functionApplicationLed _ lhs pp =
    mkFusedFunction lhs <$>
             ((withtws (pp (LAssoc 10)) `sepBy` withtws comma) <*
              withtws (char ')'))

mkFusedFunction :: Expr -> [Expr] -> Expr
mkFusedFunction (ORef obj sid) exprs = OMethod obj sid exprs
mkFusedFunction fn exprs = FunctionApplication fn exprs

orefLed :: LeftDenotation String ParseState ParseMonad Expr String
orefLed _ lhs _ = ORef lhs <$> scopedID_
                   
prefixOperatorList :: [PrefixOperatorInfo String ParseState ParseMonad Expr String]
prefixOperatorList =
    [
        SimplePrefixOperator "-" prefixOpBinder,
        SimplePrefixOperator "~" prefixOpBinder,
        SimplePrefixOperator "!" prefixOpBinder
    ]

prefixOpBinder :: PrefixBinder String ParseState ParseMonad Expr String
prefixOpBinder (SimplePrefixOperator name _) rhs = PrefixOp name rhs
prefixOpBinder _ _ = error "binder should only be called on simple prefix operators"
    
term_ :: PrecedenceParser String ParseState ParseMonad Expr -> ExprP
term_ parseSub = varRef_ <|>
                 (bracketed $ parseSub (LAssoc 0)) <|>
                 intLiteral_ <|>
                 stringLiteral_

varRef_ :: ExprP
varRef_ = fmap VarRef scopedID_

    

    
    
