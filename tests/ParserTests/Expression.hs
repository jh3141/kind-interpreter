module ParserTests.Expression (expressionParserTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Parser.ExpressionParser
import Text.Parsec
    
import ParserTests.Util
    
parseExpr :: String -> Expr
parseExpr = parseString expr_

expressionParserTests :: TestTree
expressionParserTests =
    testGroup "Expressions" [
        testCase "Variable reference" $
                 parseExpr "a" @=? VarRef "a",
        testCase "bracketed term" $
                 parseExpr "(a)" @=? VarRef "a",
        testCase "integer literal" $
                 parseExpr "123" @=? IntLiteral 123
    ]                       
