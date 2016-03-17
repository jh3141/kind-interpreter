module ParserTests.CombinatorTests (parserCombinatorTests) where

import KindLang.Internal
import Test.Tasty
import Test.Tasty.HUnit
import ParserTests.Util
import Text.Parsec.Char
import Text.Parsec

parserCombinatorTests :: TestTree
parserCombinatorTests =
    testGroup "Parser combinators" [
        testCase "</> left" $
                 parseString (letter </> many digit) "a" @=? Left 'a',
        testCase "</> right" $
                 parseString (letter </> many digit) "33" @=? Right "33"
    ]
