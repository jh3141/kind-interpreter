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
                 parseExpr "a" @?= VarRef (ASTNodeInfo 1) (UnqualifiedID "a"),
        testCase "bracketed term" $
                 parseExpr "(a)" @?= VarRef (ASTNodeInfo 2) (UnqualifiedID "a"),
        testCase "integer literal" $
                 parseExpr "123" @?= IntLiteral (ASTNodeInfo 2) 123,
        testCase "basic operators" $
                 parseExpr "a + b*2 - c" @?=
                 BinOp (ASTNodeInfo 7)
                       "-" (BinOp (ASTNodeInfo 2)
                                  "+" (VarRef (ASTNodeInfo 1) $ UnqualifiedID "a")
                                      (BinOp (ASTNodeInfo 4) "*" (VarRef (ASTNodeInfo 3) $ UnqualifiedID "b")
                                                                 (IntLiteral (ASTNodeInfo 6) 2)))
                           (VarRef (ASTNodeInfo 8) $ UnqualifiedID "c"),
        testCase "prefix operators" $
                 parseExpr "! - ~4" @?=
                 PrefixOp (ASTNodeInfo 1) "!" (PrefixOp (ASTNodeInfo 2) "-" (PrefixOp (ASTNodeInfo 3) "~" (IntLiteral (ASTNodeInfo 4) 4))),
        testCase "function application" $
                 parseExpr "fname (1)" @?=
                 FunctionApplication (ASTNodeInfo 4)
                                         (VarRef (ASTNodeInfo 1) $ UnqualifiedID "fname")
                                         [IntLiteral (ASTNodeInfo 3) 1],
        testCase "function application with funky whitespace" $
                 parseExpr "fname(  1   )" @?=
                 FunctionApplication (ASTNodeInfo 4)
                                         (VarRef (ASTNodeInfo 1) $ UnqualifiedID "fname")
                                         [IntLiteral (ASTNodeInfo 3) 1],
        testCase "function application with multiple parameters" $
                 parseExpr "fname (1,2,3)" @?=
                 FunctionApplication (ASTNodeInfo 8)
                                 (VarRef (ASTNodeInfo 1) $ UnqualifiedID "fname")
                                 [IntLiteral (ASTNodeInfo 3) 1, IntLiteral (ASTNodeInfo 5) 2, IntLiteral (ASTNodeInfo 7) 3],
        testCase "comma operator" $
                 parseExpr "a,b" @?= BinOp (ASTNodeInfo 2) ","
                               (VarRef (ASTNodeInfo 1) $ UnqualifiedID "a")
                               (VarRef (ASTNodeInfo 3) $ UnqualifiedID "b"),
        testCase "dot operator" $
                 parseExpr "a.b" @?=
                 ORef (ASTNodeInfo 2) (VarRef (ASTNodeInfo 1) $ UnqualifiedID "a") (UnqualifiedID "b"),
        testCase "object method call fusion" $
                 parseExpr "a.b(c)" @?=
                 OMethod (ASTNodeInfo 2) (VarRef (ASTNodeInfo 1) $ UnqualifiedID "a") (UnqualifiedID "b")
                             [VarRef (ASTNodeInfo 3) $ UnqualifiedID "c"],
        testCase "string literal" $
                 parseExpr "\"hello\"" @?= StringLiteral (ASTNodeInfo 3) "hello",
        testCase "function partial and left application" $
                 parseExpr "\"world\" -> testfn\\\"hello\"" @?=
                 FunctionApplication (ASTNodeInfo 9)
                     (BinOp (ASTNodeInfo 5)
                                "\\" (VarRef (ASTNodeInfo 4) $ UnqualifiedID "testfn")
                                     (StringLiteral (ASTNodeInfo 8) "hello"))
                     [StringLiteral (ASTNodeInfo 3) "world"]
                     
    ]                       
