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
import KindLang.Data.Catalogue
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
expectRight (Left err) = error (show err)
expectRight (Right r)  = return r

nullModuleLoader :: NSID -> KStat s Catalogue
nullModuleLoader (UnqualifiedID "kind") = return coreTypes
nullModuleLoader _ = throwError $ InternalError "module loading not available"

bootstrapExpr :: Expr
bootstrapExpr = (FunctionApplication (VarRef $ UnqualifiedID "test") [])

failException :: SomeException -> IO ()
failException e = error $ displayException e

runTest :: String -> Value -> Assertion
runTest filename expected =
    do
      source <- readFile ("kindtests/ParseAndEvaluate/" ++ filename ++ ".k")
      modTree <- expectRight $ parse module_ filename source

      --putStrLn ("Test running: " ++ filename)
      --putStrLn "Parsed module: "
      --print modTree

      value <- runToIO $ do
          resolvedModule <- resolveModule modTree scopeDefault nullModuleLoader

          -- traceShowM resolvedModule

          catalogues <- buildCatalogues nullModuleLoader resolvedModule

          -- traceShowM catalogues

          let moduleScope = (makeModuleScope scopeDefault catalogues)
          resolvedBootstrap <- resolveExpr moduleScope bootstrapExpr
          -- traceShowM resolvedBootstrap

          evalAExpr (newRuntimeScope moduleScope)
                    standardInternalFunctions
                    resolvedBootstrap
      assertEqual "Returned value" expected value

makeTest :: String -> Value -> TestTree
makeTest f v = testCase f $ runTest f v

parseAndEvaluateTests :: TestTree
parseAndEvaluateTests =
    testGroup "Parse and Evaluate" $
                  (makeTest "return42" $ makeKindInt 42) :
                  (makeTest "explicit_types" $ makeKindInt 42) :
                  (makeTest "variables" $ makeKindInt 42) :
                  (makeTest "functions_and_operators" $ makeKindInt 42) :
                  []