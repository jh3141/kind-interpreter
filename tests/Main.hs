module Main where

import Test.Tasty
import ParserTests.Module 
import ParserTests.CombinatorTests
    
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
         testGroup "Parser" $
                   moduleParserTests :
                   parserCombinatorTests :
                   []
        ]

