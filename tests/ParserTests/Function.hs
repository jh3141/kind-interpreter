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
                 (parseFn "a(b,c) {}")  @?=
                 (FunctionInstance
                  (FunctionType [InferableType, InferableType] InferableType)
                  ["b","c"]
                  (StatementBlock [])),
        testCase "Parameter definitions with types" $
                 (parseFn "a (b : Module::Type) {}")  @?=
                 (FunctionInstance
                  (FunctionType [SimpleType (QualifiedID "Module" $
                                             UnqualifiedID "Type")] InferableType)
                  ["b"] (StatementBlock [])),
        testCase "Return type" $
                 (parseFn "a () : Out {}") @?=
                 (FunctionInstance
                  (FunctionType [] (SimpleType $ UnqualifiedID "Out"))
                  [] (StatementBlock [])),
        testCase "Unspecified return type" $
                 (parseFn "a(){}") @?=
                 (FunctionInstance
                  (FunctionType [] InferableType) [] (StatementBlock [])),
        testCase "Body" $
                 (fnInstanceBody $ parseFn "a(b){b;}") @?=
                 (Left $ Expression $ VarRef idb),
        testCase "Body with two expressions" $
                 (fnInstanceBody $ parseFn "a(b,c){b;c;}") @?=
                 (Left $ StatementBlock [Expression $ VarRef idb,
                                         Expression $ VarRef idc]),
        testCase "Function with multiple instances" $
                 (parseFnM "a(b){b;},(b,c){b+c;} , (b,c,d){b+c+d;}") @?=
                 [FunctionInstance (FunctionType [InferableType] InferableType)
                                   ["b"] (Expression $ VarRef idb),
                  FunctionInstance (FunctionType [InferableType,InferableType]
                                                 InferableType)
                                   ["b","c"]
                                   (Expression $ BinOp "+"
                                               (VarRef idb) (VarRef idc)),
                  FunctionInstance (FunctionType (replicate 3 InferableType)
                                                 InferableType)
                                   ["b","c","d"]
                                   (Expression $ BinOp "+"
                                               (BinOp "+" (VarRef idb)
                                                          (VarRef idc))
                                               (VarRef idd))]
    ]                         

idb :: NSID
idb = UnqualifiedID "b"
idc :: NSID
idc = UnqualifiedID "c"
idd :: NSID
idd = UnqualifiedID "d"
