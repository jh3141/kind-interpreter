module ParserTests.Expression (expressionParserTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Parser.ExpressionParser
    
import ParserTests.Util
    
parseExpr :: String -> Expr
parseExpr = parseString expr_

expressionParserTests :: TestTree
expressionParserTests =
    testGroup "Expressions" [
        testCase "Variable reference" $
                 parseExpr "a" @?= VarRef (UnqualifiedID "a"),
        testCase "bracketed term" $
                 parseExpr "(a)" @?= VarRef (UnqualifiedID "a"),
        testCase "integer literal" $
                 parseExpr "123" @?= IntLiteral 123,
        testCase "basic operators" $
                 parseExpr "a + b*2 - c" @?=
                 BinOp "-" (BinOp "+" (VarRef $ UnqualifiedID "a")
                                      (BinOp "*" (VarRef $ UnqualifiedID "b")
                                                 (IntLiteral 2)))
                           (VarRef $ UnqualifiedID "c"),
        testCase "prefix operators" $
                 parseExpr "! - ~4" @?=
                 PrefixOp "!" (PrefixOp "-" (PrefixOp "~" (IntLiteral 4))),
        testCase "function application" $
                 parseExpr "fname (1)" @?=
                 FunctionApplication (VarRef $ UnqualifiedID "fname")
                                         [IntLiteral 1],
        testCase "function application with funky whitespace" $
                 parseExpr "fname(  1   )" @?=
                 FunctionApplication (VarRef $ UnqualifiedID "fname")
                                         [IntLiteral 1],
        testCase "function application with multiple parameters" $
                 parseExpr "fname (1,2,3)" @?=
                 FunctionApplication (VarRef $ UnqualifiedID "fname")
                                 [IntLiteral 1, IntLiteral 2, IntLiteral 3],
        testCase "comma operator" $
                 parseExpr "a,b" @?= BinOp "," (VarRef $ UnqualifiedID "a")
                               (VarRef $ UnqualifiedID "b"),
        testCase "dot operator" $
                 parseExpr "a.b" @?=
                 ORef (VarRef $ UnqualifiedID "a") (UnqualifiedID "b"),
        testCase "object method call fusion" $
                 parseExpr "a.b(c)" @?=
                 OMethod (VarRef $ UnqualifiedID "a") (UnqualifiedID "b")
                             [VarRef $ UnqualifiedID "c"],
        testCase "string literal" $
                 parseExpr "\"hello\"" @?= StringLiteral "hello",
        testCase "function partial and left application" $
                 parseExpr "\"world\" -> testfn\\\"hello\"" @?=
                 FunctionApplication
                     (BinOp "\\" (VarRef $ UnqualifiedID "testfn")
                                (StringLiteral "hello"))
                     [StringLiteral "world"]
                     
    ]                       
