module ParserTests.Util where

import Test.Tasty.HUnit
import Text.Parsec
import Control.Arrow
import Data.List
    
parseString :: Parsec String () r -> String -> r
parseString parser toParse =
    either (show >>> error) id $
           parse (parser <* eof) "test" toParse

    
expectParseError :: Show r => Parsec String () r -> String -> String -> Assertion
expectParseError parser toParse expectedErrorText =
    case (parse (parser <* eof) "test" toParse) of
     Left err -> assertBool
                   ("Expected error " ++
                     (show expectedErrorText) ++ " but got " ++
                     (show err))
                   (expectedErrorText `isSubsequenceOf` show err)
     Right res ->
        assertFailure ("Expected error, but received result: " ++ (show res))
      

                  
                
