module ParserTests.Module where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Parser.ModuleParser    
import KindLang.Data.AST
import Text.Parsec
    
parseString :: Parsec String () r -> String -> r
parseString parser string = case parse parser "test" string of
                              Right x -> x
                              Left  e -> error $ show e
                                    
moduleParserTests :: TestTree
moduleParserTests =
    testGroup "Module parser" [
        testCase "empty module" $
                 parseString _module_ "" @=? Module Nothing [] []      
    ]                     
