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
                 parseString (letter </> many digit) "a" @?= Left 'a',
        testCase "</> right" $
                 parseString (letter </> many digit) "33" @?= Right "33",
        testCase "withtws" $
                 parseString (withtws letter <* digit) "a   1" @?= 'a',
        testCase "withlws" $
                 parseString (letter *> withlws digit) "a   1" @?= '1',
        testCase "withws" $
                 parseString (letter *> withws digit <* letter) "a  1 n"
                 @?= '1',
        testCase "withws without whitespace" $
                 parseString (letter *> withws digit <* letter) "a1n"
                 @?= '1',
        testCase "<:>" $
                 parseString (letter <:> many digit) "a12"
                 @?= "a12",
        testCase "<::>" $
                 parseString (letter <::> digit) "a1"
                 @?= "a1",
        testCase "building complex list with <:> and <::>" $
                 parseString (letter <:> letter<|>digit <::> digit) "aa2"
                 @?= "aa2",                     
        testCase "sepBy1Lazy not followed by separator" $
                 parseString (sepBy1Lazy digit space <::> many anyToken)
                     "1 2 3ab" @?= ["123","ab"],
        testCase "sepBy1Lazy followed by separator" $
                 parseString (sepBy1Lazy digit space <::> many anyToken)
                     "1 2 3 ab" @?= ["123"," ab"]                      
    ]
