module Main where

import Test.Tasty
import ParserTests.Module (moduleParserTests)

    
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
         testGroup "Parser" [ moduleParserTests ]
        ]

