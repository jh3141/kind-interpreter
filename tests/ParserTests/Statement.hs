module ParserTests.Statement (statementParserTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Parser.StatementParser
import Text.Parsec
    
import ParserTests.Util

parseStmt :: String -> Statement
parseStmt = parseString stmt_

statementParserTests :: TestTree
statementParserTests =
    testGroup "Statements"
    [
        testCase "Expressions can be statements" $
                 parseStmt "2+2;" @?=
                 Expression (BinOp "+" (IntLiteral 2) (IntLiteral 2)),
        testGroup "Variable declarations"
        [
            testCase "With inferred type and init expression" $
                     parseStmt "var := 1;" @?=
                     VarDeclStatement "var"
                                      InferableType
                                      (VarInitExpr $ IntLiteral 1),
            testCase "With explicit type and init expression" $
                     parseStmt "var:int=1;" @?=
                     VarDeclStatement "var"
                                      (SimpleType $ UnqualifiedID "int")
                                      (VarInitExpr $ IntLiteral 1),
            testCase "With explicit type and no init" $
                     parseStmt "var:int;" @?=
                     VarDeclStatement "var"
                                      (SimpleType $ UnqualifiedID "int")
                                      VarInitNone,
            testCase "With explicit type and constructor" $
                     parseStmt "var : Vector3D (5,6,0);" @?=
                     VarDeclStatement "var"
                                      (SimpleType $ UnqualifiedID "Vector3D")
                                      (VarInitConstruct
                                          [ IntLiteral 5,
                                            IntLiteral 6,
                                            IntLiteral 0 ]),
            testCase "Cannot infer type with no init" $
                     expectParseError stmt_ "var:;" "",
            testCase "Cannot infer type with constructor call" $
                     expectParseError stmt_ "var:(1);" ""
        ]
    ]
    
