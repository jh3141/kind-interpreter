module ParserTests.Module (moduleParserTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Parser.ModuleParser    
import KindLang.Data.AST
import Text.Parsec
    
import ParserTests.Util
    
parseMod :: String -> Module
parseMod = parseString module_
           
moduleParserTests :: TestTree
moduleParserTests =
    testGroup "Module parser" [
        testCase "empty module" $
                 parseMod "" @?= Module Nothing [] [],
        testCase "module line sets name" $
                 parseMod "module ModuleName;" @?=
                          Module (Just $ UnqualifiedID "ModuleName") [] [],
        testCase "module name can be scoped" $
                 parseMod "module ModuleName::SubName::SubSubName;" @?=
                          Module (Just $ QualifiedID "ModuleName"
                                       $ QualifiedID "SubName"
                                       $ UnqualifiedID "SubSubName")
                                 [] [],
        testCase "import declaration parsed" $
                 parseMod "import id1::id2;" @?=
                          Module Nothing [UnqualifiedModuleImport
                                              (QualifiedID "id1" $
                                               UnqualifiedID "id2") False] [],
        testCase "whole-namespace import declaration parsed" $
                 parseMod "import id1::*;" @?=
                          Module Nothing [UnqualifiedModuleImport
                                              (UnqualifiedID "id1") True] [],

        -- fixme:
        -- testGroup "Import error handling" [
           -- testCase "error if import not followed by id"
           -- testCase "error if non-identifier in imported scoped id"
           -- testCase "error if EOF after import"
           -- testCase "error if EOF after import id"
           -- testCase "error on import *"
           -- testCase "resync after 'import *;'
           -- testCase "error if no semicolon after import statement"
           -- testCase "error if wildcard in middle of scoped id"
        -- ]

        -- fixme: import qualified
        testCase "module name and several import lines" $
                 parseMod "module a;\nimport b::*;import c::*;" @?=
                          Module (Just $ UnqualifiedID "a")
                                 [UnqualifiedModuleImport
                                      (UnqualifiedID "b") True,
                                  UnqualifiedModuleImport
                                      (UnqualifiedID "c") True] [],
        testCase "variable declaration" $
                 parseMod "varname : T;" @?=
                          Module Nothing []
                                 [("varname",
                                   VariableDefinition
                                       (SimpleType $ UnqualifiedID "T")
                                       VarInitNone)],
        testCase "variable declaration with initialiser" $
                 parseMod "varname : T = a;" @?=
                          Module Nothing []
                                 [("varname",
                                   VariableDefinition
                                       (SimpleType $ UnqualifiedID "T")
                                       (VarInitExpr $ VarRef "a"))],
        testCase "variable declaration with constructor" $
                 parseMod "varname : T(a,b);" @?=
                          Module Nothing []
                                 [("varname",
                                   VariableDefinition
                                       (SimpleType $ UnqualifiedID "T")
                                       (VarInitConstruct [VarRef "a",
                                                          VarRef "b"]))],
        testCase "function declaration" $
                 parseMod "testFunction(){}" @?=
                          Module Nothing []
                                 [("testFunction",
                                   FunctionDefinition [] InferableType [])]
                                   
                                  
    ]                     
