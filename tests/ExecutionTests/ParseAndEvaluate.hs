{-# LANGUAGE RankNTypes #-}
module ExecutionTests.ParseAndEvaluate (parseAndEvaluateTests, runTest) where

import Control.Exception
import Control.Monad.Except
import Debug.Trace
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Parser.ModuleParser
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.Value
import KindLang.Data.Scope
import KindLang.Analysis.BuildCatalogue
import KindLang.Analysis.ResolveTypes
import KindLang.Data.KStat
import KindLang.Runtime.Eval
import KindLang.Lib.CoreTypes
import KindLang.Lib.Operators
import Text.Parsec

expectRight :: Show a => Either a b -> IO b
expectRight (Left err) = error $ "Unexpected failure: " ++ (show err)
expectRight (Right r)  = return r

bootstrapExpr :: Expr
bootstrapExpr = (FunctionApplication (VarRef $ UnqualifiedID "test") [])

failException :: SomeException -> IO ()
failException e = error $ displayException e

runTest :: String -> (forall s . Value s) -> Assertion
runTest filename expected =
    do
      source <- readFile ("kindtests/ParseAndEvaluate/" ++ filename ++ ".k")
      modTree <- expectRight $ parse module_ filename source

      --putStrLn ("Test running: " ++ filename)
      --putStrLn "Parsed module: "
      --print modTree

      expectNoErrors "unexpected error during execution" $ do
          scope <- (scopeDefault >>= addStandardOperatorsToScope)
          resolvedModule <- resolveModule modTree scope nullModuleLoader

          -- traceShowM resolvedModule

          moduleScope <- buildScope nullModuleLoader scope resolvedModule
                                    instantiateScopeDefinitions

          -- traceShowM moduleScope

          resolvedBootstrap <- resolveExpr moduleScope bootstrapExpr
          -- traceShowM resolvedBootstrap

          kstatSetInternalFunctions standardInternalFunctions

          value <- evalAExpr moduleScope resolvedBootstrap >>= refToValue

          return $ assertEqual "Returned value" expected value

makeTest :: String -> (forall s . Value s) -> TestTree
makeTest f v = testCase f $ runTest f v

parseAndEvaluateTests :: TestTree
parseAndEvaluateTests =
    testGroup "Parse and Evaluate" $
                  (makeTest "return42" $ makeKindInt 42) :
                  (makeTest "explicit_types" $ makeKindInt 42) :
                  (makeTest "variables" $ makeKindInt 42) :
                  (makeTest "functions_and_operators" $ makeKindInt 42) :
                  (makeTest "functions2" $ makeKindInt 42) :
                  (makeTest "overloadedfunctions" $ makeKindInt 42) :
                  (makeTest "function_inference" $ makeKindInt 42) :
                  (makeTest "lvalue_operators" $ makeKindInt 42) :
                  []
