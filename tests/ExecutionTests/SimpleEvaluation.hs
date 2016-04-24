module ExecutionTests.SimpleEvaluation (simpleEvaluationTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Scope
import KindLang.Data.Value
import KindLang.Data.Catalogue
import KindLang.Lib.CoreTypes
import KindLang.Runtime.Eval
import KindLang.Analysis.ResolveTypes

-- evaluate expression resolved against scope and extract from error wrapper
execTest :: Scope -> Expr -> Value
execTest s ex = either
                 (\e -> error (show e)) -- if there's an error
                 id                     -- otherwise
                 ((resolveExpr s ex) >>= evalAExpr)
    
simpleEvaluationTests :: TestTree
simpleEvaluationTests =
    testGroup "Simple evaluation" (
        (testCase "Evaluate an integer literal" $
                  (getKindInt (execTest testScope $ IntLiteral 13)) @?= 13) :
        (testCase "Evaluate no-args internal function" $
                  (getKindInt (execTest testScope $
                    FunctionApplication (VarRef idRet42) []))
                  @?= 42) :
        [])

testScope :: Scope
testScope = (Scope Nothing newCatalogue)
            |@+| ("ret42", FunctionDefinition [
                              InternalFunction
                                (FunctionType [] rtKindInt)
                                (PrintableFunction "ret42" (const $ makeKindInt 42))])
              
                  
idRet42 :: NSID
idRet42 = listToNSID ["ret42"]
          
