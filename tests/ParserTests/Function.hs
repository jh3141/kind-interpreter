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
                         [("b", InferableType), ("c", InferableType)]
    ]                         
