module ExecutionTests.SimpleEvaluation where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Data.Scope
import KindLang.Data.Value
import KindLang.Data.Catalogue
import KindLang.Lib.CoreTypes
import KindLang.Runtime.Eval
import KindLang.Analysis.ResolveTypes

simpleEvalutationTests :: TestTree
simpleEvalutationTests =
    testGroup "Simple evaluation"
        (testCase "Evaluate no-args internal function" $
                  (kindGetInt (evalAExpr $ resolveExpr testScope $
                               FunctionApplication (VarRef idRet42) [])) ?@= 42) :
        []

testScope :: Scope
testScope = (Scope Nothing newCatalogue)
            |@+| ("idRet42", FunctionDefinition [
                                InternalFunction
                                  (FunctionType [] rtKindInt)
                                  (\ values -> kindMakeInt 42)])
              
                  
