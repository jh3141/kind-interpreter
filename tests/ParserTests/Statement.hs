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
    testGroup "Statements" [
        testCase "Expressions can be statements" $
                 parseStmt "2+2;" @?=
                 Expression (BinOp "+" (IntLiteral 2) (IntLiteral 2))
    ]
    
