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
                 parseExpr "123" @=? IntLiteral 123,
        testCase "basic operators" $
                 parseExpr "a + b*2 - c" @=?
                 BinOp "-" (BinOp "+" (VarRef "a")
                                      (BinOp "*" (VarRef "b")
                                                 (IntLiteral 2)))
                           (VarRef "c"),
        testCase "prefix operators" $
                 parseExpr "! - ~4" @=?
                 PrefixOp "!" (PrefixOp "-" (PrefixOp "~" (IntLiteral 4))),
        testCase "function application" $
                 parseExpr "fname (1)" @=?
                 FunctionApplication (VarRef "fname") [IntLiteral 1],
        testCase "function application with funky whitespace" $
                 parseExpr "fname(  1   )" @=?
                 FunctionApplication (VarRef "fname") [IntLiteral 1],
        testCase "function application with multiple parameters" $
                 parseExpr "fname (1,2,3)" @=?
                 FunctionApplication (VarRef "fname")
                                 [IntLiteral 1, IntLiteral 2, IntLiteral 3],
        testCase "comma operator" $
                 parseExpr "a,b" @=? BinOp "," (VarRef "a") (VarRef "b"),
        testCase "dot operator" $
                 parseExpr "a.b" @=?
                 ORef (VarRef "a") (UnqualifiedID "b"),
        testCase "object method call fusion" $
                 parseExpr "a.b(c)" @=?
                 OMethod (VarRef "a") (UnqualifiedID "b") [VarRef "c"]
    ]                       
