{-# LANGUAGE RankNTypes #-}

module ParserTests.Util where

import Test.Tasty.HUnit
import Text.Parsec
import Control.Arrow
import Data.List

import KindLang.Data.KStat
    
parseString :: (forall s . ParsecT String () (KStat s) r) -> String -> r
parseString parser toParse =
    either (show >>> error) id $
           either (show >>> error) id $ runToEither $
                  runParserT (parser <* eof) () "test" toParse

    
expectParseError :: Show r => (forall s . ParsecT String () (KStat s) r) -> String -> String -> Assertion
expectParseError parser toParse expectedErrorText =
    case (runToEither $ runParserT (parser <* eof) () "test" toParse) of
     Left err -> assertFailure ("Expected parse error, but received execution error: " ++ (show err))
     Right (Left err) -> assertBool
                           ("Expected error " ++
                            (show expectedErrorText) ++ " but got " ++
                            (show err))
                           (expectedErrorText `isSubsequenceOf` show err)
     Right (Right res) ->
        assertFailure ("Expected parse error, but received result: " ++ (show res))
      

                  
                
