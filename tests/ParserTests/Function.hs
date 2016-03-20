module ParserTests.Function (functionParserTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Parser.ModuleParser    
import KindLang.Data.AST
import Text.Parsec
import Control.Arrow
    
import ParserTests.Util

parseFn :: String -> Definition
parseFn = parseString functionDeclaration_ >>> snd

functionParserTests :: TestTree
functionParserTests =
    testGroup "Function definitions" [
        testCase "Parameter definitions" $
                 (fnDefParams $ parseFn "a(b,c) {}")  @=?
                         [("b", InferableType), ("c", InferableType)],
        testCase "Parameter definitions with types" $
                 (fnDefParams $ parseFn "a (b : Module::Type) {}")  @=?
                 [("b", SimpleType (QualifiedID "Module" $
                                    UnqualifiedID "Type"))],
        testCase "Return type" $
                 (fnDefReturnType $ parseFn "a () : Out {}") @=?
                 (SimpleType $ UnqualifiedID "Out"),
        testCase "Unspecified return type" $
                 (fnDefReturnType $ parseFn "a(){}") @=? InferableType,
        testCase "Body" $
                 (fnDefBody $ parseFn "a(b){b;}") @=? [VarRef "b"],
        testCase "Body with two expressions" $
                 (fnDefBody $ parseFn "a(b,c){b;c;}") @=?
                 [VarRef "b", VarRef "c"]
    ]                         
