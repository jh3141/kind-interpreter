{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes, ImpredicativeTypes, LiberalTypeSynonyms #-}

module KindLang.Parser.ExpressionParser where

import Control.Monad.Trans
import Text.Parsec
    
import KindLang.Data.AST
import KindLang.Util.Control
import KindLang.Parser.Combinators
import KindLang.Parser.BasicTokens
import KindLang.Parser.State
import Text.Parsec.PrattParser

type ExprP = Parser Expr
type ExprPS s = ParserS s Expr
    
type LeftDenotationExpr s = LeftDenotation String ParseState (ParseMonad s) Expr String
type OperatorInfoExpr s = OperatorInfo String ParseState (ParseMonad s) Expr String
    
expr_ :: ExprP
expr_ = buildPrattParser
          operatorList prefixOperatorList whitespace_ operator_ term_

operatorList :: forall s . [OperatorInfoExpr s]
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

binOpLed :: forall s . LeftDenotationExpr s
binOpLed (OperatorInfo name prec _) lhs pp = (newNodeP BinOp) $# name $# lhs <*> pp prec

reverseFunctionApplicationLed :: forall s . LeftDenotationExpr s
reverseFunctionApplicationLed (OperatorInfo _ prec _) lhs pp =
     pp prec >>= \ rhs -> lift $ mkFusedFunction rhs [lhs]
                                         
functionApplicationLed :: forall s . LeftDenotationExpr s
functionApplicationLed _ lhs pp =
    ((withtws (pp (LAssoc 10)) `sepBy` withtws comma) <* withtws (char ')')) >>=
      \ rhs -> lift $ mkFusedFunction lhs rhs


mkFusedFunction :: forall s . Expr -> [Expr] -> ParseMonad s Expr
mkFusedFunction (ORef ni obj sid) exprs = return $ OMethod ni obj sid exprs
mkFusedFunction fn exprs = newNode FunctionApplication $# fn $# exprs

orefLed :: forall s . LeftDenotationExpr s
orefLed _ lhs _ = newNodeP ORef $# lhs <*> scopedID_
                   
prefixOperatorList :: forall s . [PrefixOperatorInfo String ParseState (ParseMonad s) Expr String]
prefixOperatorList =
    [
        SimplePrefixOperator "-" prefixOpBinder,
        SimplePrefixOperator "~" prefixOpBinder,
        SimplePrefixOperator "!" prefixOpBinder
    ]

prefixOpBinder :: forall s . PrefixBinder String ParseState (ParseMonad s) Expr String
prefixOpBinder (SimplePrefixOperator name _) rhs = newNode PrefixOp $# name $# rhs
prefixOpBinder _ _ = error "binder should only be called on simple prefix operators"
    
term_ :: forall s . PrecedenceParser String ParseState (ParseMonad s) Expr -> ExprPS s
term_ parseSub = varRef_ <|>
                 (bracketed $ parseSub (LAssoc 0)) <|>
                 intLiteral_ <|>
                 stringLiteral_

varRef_ :: ExprP
varRef_ = newNodeP VarRef <*> scopedID_

    

    
    
