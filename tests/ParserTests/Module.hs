module ParserTests.Module where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Parser.ModuleParser    
import KindLang.Data.AST
import Text.Parsec
    
parseString :: Parsec String () r -> String -> r
parseString parser toParse = case parse parser "test" toParse of
                               Right x -> x
                               Left  e -> error $ show e
parseMod :: String -> Module
parseMod = parseString _module_
           
moduleParserTests :: TestTree
moduleParserTests =
    testGroup "Module parser" [
        testCase "empty module" $
                 parseMod "" @=? Module Nothing [] [],
        testCase "module line sets name" $
                 parseMod "module ModuleName;" @=?
                          Module (Just $ UnqualifiedID "ModuleName") [] [],
        testCase "module name can be scoped" $
                 parseMod "module ModuleName::SubName::SubSubName;" @=?
                          Module (Just $ QualifiedID "ModuleName"
                                       $ QualifiedID "SubName"
                                       $ UnqualifiedID "SubSubName")
                                 [] [],
        testCase "import declaration parsed" $
                 parseMod "import id1::id2;" @=?
                          Module Nothing [UnqualifiedModuleImport
                                              (QualifiedID "id1" $
                                               UnqualifiedID "id2") False] [],
        testCase "whole-namespace import declaration parsed" $
                 parseMod "import id1::*;" @=?
                          Module Nothing [UnqualifiedModuleImport
                                              (UnqualifiedID "id1") True] []
    ]                     
