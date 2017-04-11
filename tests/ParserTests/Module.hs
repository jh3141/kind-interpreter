module ParserTests.Module (moduleParserTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Parser.ModuleParser    
import KindLang.Data.AST
import KindLang.Data.BasicTypes   
import KindLang.Data.KStat
import KindLang.Data.Types
    
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
                          Module Nothing [UnqualifiedModuleImport (ASTNodeInfo 1)
                                              (QualifiedID "id1" $
                                               UnqualifiedID "id2") False] [],
        testCase "whole-namespace import declaration parsed" $
                 parseMod "import id1::*;" @?=
                          Module Nothing [UnqualifiedModuleImport (ASTNodeInfo 1)
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
                                 [UnqualifiedModuleImport (ASTNodeInfo 1)
                                      (UnqualifiedID "b") True,
                                  UnqualifiedModuleImport (ASTNodeInfo 2)
                                      (UnqualifiedID "c") True] [],
        testCase "variable declaration" $
                 parseMod "varname : T;" @?=
                          Module Nothing []
                                 [("varname",
                                   VariableDefinition (ASTNodeInfo 1)
                                       (SimpleType $ UnqualifiedID "T")
                                       VarInitNone)],
        testCase "variable declaration with initialiser" $
                 parseMod "varname : T = a;" @?=
                          Module Nothing []
                                 [("varname",
                                   VariableDefinition (ASTNodeInfo 2)
                                     (SimpleType $ UnqualifiedID "T")
                                     (VarInitExpr $ VarRef (ASTNodeInfo 1) (UnqualifiedID "a")))],
        testCase "variable declaration with constructor" $
                 parseMod "varname : T(a,b);" @?=
                          Module Nothing []
                                 [("varname",
                                   VariableDefinition (ASTNodeInfo 4)
                                     (SimpleType $ UnqualifiedID "T")
                                     (VarInitConstruct [
                                       VarRef (ASTNodeInfo 1) $ UnqualifiedID "a",
                                       VarRef (ASTNodeInfo 3) $ UnqualifiedID "b"]))],
        testCase "function declaration" $
                 parseMod "testFunction(){}" @?=
                          Module Nothing []
                                 [("testFunction",
                                   FunctionDefinition (ASTNodeInfo 1) [
                                    FunctionInstance (ASTNodeInfo 2)
                                      (FunctionType [] InferableType)
                                      [] (StatementBlock (ASTNodeInfo 8) [])])],

        testCase "End-of-line comments stripped" $
                 parseMod "// comment\nmodule MyName;// another" @?=
                          Module (Just $ UnqualifiedID "MyName") [] []
                                  
    ]                     
