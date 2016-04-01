module Main where

import Test.Tasty
import ParserTests.Module 
import ParserTests.CombinatorTests
import ParserTests.Function
import ParserTests.Expression
import ParserTests.Statement
import AnalysisTests.Catalogue
import AnalysisTests.TypeResolution
    
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
         testGroup "Parser" $
                   moduleParserTests :
                   parserCombinatorTests :
                   functionParserTests :
                   expressionParserTests :
                   statementParserTests :
                   [],
         testGroup "Analysis" $
                   catalogueTests :
                   typeResolutionTests :
                   []
        ]

