module ParserTests.Function (functionParserTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Parser.ModuleParser    
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import Text.Parsec
import Control.Arrow
    
import ParserTests.Util

parseFn :: String -> FunctionInstance
parseFn = parseFnM >>> head

parseFnM :: String -> [FunctionInstance]
parseFnM = parseString functionDeclaration_ >>> snd >>> fnDefInstances

functionParserTests :: TestTree
functionParserTests =
    testGroup "Function definitions" [
        testCase "Parameter definitions" $
                 (fnDefParams $ parseFn "a(b,c) {}")  @?=
                         [("b", InferableType), ("c", InferableType)],
        testCase "Parameter definitions with types" $
                 (fnDefParams $ parseFn "a (b : Module::Type) {}")  @?=
                 [("b", SimpleType (QualifiedID "Module" $
                                    UnqualifiedID "Type"))],
        testCase "Return type" $
                 (fnDefReturnType $ parseFn "a () : Out {}") @?=
                 (SimpleType $ UnqualifiedID "Out"),
        testCase "Unspecified return type" $
                 (fnDefReturnType $ parseFn "a(){}") @?= InferableType,
        testCase "Body" $
                 (fnDefBody $ parseFn "a(b){b;}") @?=
                 [Expression $ VarRef "b"],
        testCase "Body with two expressions" $
                 (fnDefBody $ parseFn "a(b,c){b;c;}") @?=
                 [Expression $ VarRef "b", Expression $ VarRef "c"],
        testCase "Function with multiple instances" $
                 (parseFnM "a(b){b;},(b,c){b+c;} , (b,c,d){b+c+d;}") @?=
                 [FunctionInstance [("b",InferableType)]
                                   InferableType
                                   [Expression $ VarRef "b"],
                  FunctionInstance [("b",InferableType),("c",InferableType)]
                                   InferableType
                                   [Expression $ BinOp "+"
                                               (VarRef "b") (VarRef "c")],
                  FunctionInstance [("b",InferableType),("c",InferableType),
                                    ("d",InferableType)]
                                   InferableType
                                   [Expression $ BinOp "+"
                                               (BinOp "+" (VarRef "b")
                                                          (VarRef "c"))
                                               (VarRef "d")]]
    ]                         
