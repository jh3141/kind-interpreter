module ParserTests.Function (functionParserTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Parser.ModuleParser    
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Types
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
                 (FunctionInstance (ASTNodeInfo 2)
                  (FunctionType [InferableType, InferableType] InferableType)
                  ["b","c"]
                  (StatementBlock (ASTNodeInfo 8) [])),
        testCase "Parameter definitions with types" $
                 (parseFn "a (b : Module::Type) {}")  @?=
                 (FunctionInstance (ASTNodeInfo 2)
                  (FunctionType [SimpleType (QualifiedID "Module" $
                                             UnqualifiedID "Type")] InferableType)
                  ["b"] (StatementBlock (ASTNodeInfo 8) [])),
        testCase "Return type" $
                 (parseFn "a () : Out {}") @?=
                 (FunctionInstance (ASTNodeInfo 2)
                  (FunctionType [] (SimpleType $ UnqualifiedID "Out"))
                  [] (StatementBlock (ASTNodeInfo 8) [])),
        testCase "Unspecified return type" $
                 (parseFn "a(){}") @?=
                 (FunctionInstance (ASTNodeInfo 2)
                  (FunctionType [] InferableType) [] (StatementBlock (ASTNodeInfo 8) [])),
        testCase "Body" $
                 (fnInstanceBody $ parseFn "a(b){b;}") @?=
                 (Expression (ASTNodeInfo 4) $ VarRef (ASTNodeInfo 5) idb),
        testCase "Body with two expressions" $
                 (fnInstanceBody $ parseFn "a(b,c){b;c;}") @?=
                 (StatementBlock (ASTNodeInfo 14)
                                 [Expression (ASTNodeInfo 4) $ VarRef (ASTNodeInfo 5) idb,
                                  Expression (ASTNodeInfo 7) $ VarRef (ASTNodeInfo 8) idc]),
        testCase "Function with multiple instances" $
                 (parseFnM "a(b){b;},(b,c){b+c;} , (b,c,d){b+c+d;}") @?=
                 [FunctionInstance (ASTNodeInfo 2) (FunctionType [InferableType] InferableType)
                                   ["b"] (Expression (ASTNodeInfo 4) $ VarRef (ASTNodeInfo 5) idb),
                  FunctionInstance (ASTNodeInfo 11) (FunctionType [InferableType,InferableType]
                                                 InferableType)
                                   ["b","c"]
                                   (Expression (ASTNodeInfo 13)
                                               (BinOp (ASTNodeInfo 15) "+"
                                                      (VarRef (ASTNodeInfo 14) idb) (VarRef (ASTNodeInfo 16) idc))),
                  FunctionInstance (ASTNodeInfo 22) (FunctionType (replicate 3 InferableType)
                                                                  InferableType)
                                   ["b","c","d"]
                                   (Expression (ASTNodeInfo 24) $
                                               BinOp (ASTNodeInfo 28) "+"
                                                   (BinOp (ASTNodeInfo 26)
                                                           "+" (VarRef (ASTNodeInfo 25) idb)
                                                               (VarRef (ASTNodeInfo 27) idc))
                                                   (VarRef (ASTNodeInfo 29) idd))]
    ]                         

idb :: NSID
idb = UnqualifiedID "b"
idc :: NSID
idc = UnqualifiedID "c"
idd :: NSID
idd = UnqualifiedID "d"
