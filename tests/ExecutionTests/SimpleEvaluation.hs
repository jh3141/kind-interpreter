module ExecutionTests.SimpleEvaluation (simpleEvaluationTests) where

import qualified Data.Map as Map
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
                 ((resolveExpr s ex) >>= (evalAExpr s ifc))
    
simpleEvaluationTests :: TestTree
simpleEvaluationTests =
    testGroup "Simple evaluation" (
        (testCase "Evaluate an integer literal" $
                  (getKindInt (execTest testScope $ IntLiteral 13)) @?= 13) :
        (testCase "Evaluate no-args internal function" $
                  (getKindInt (execTest testScope $
                    FunctionApplication (VarRef idRet42) []))
                  @?= 42) :
        (testCase "Evaluate function with kind definition" $
                  (getKindInt (execTest testScope $
                     FunctionApplication (VarRef idRet43) []))
                  @?= 43) :
        (testCase "Evaluate function with arguments" $
                  (getKindInt (execTest testScope $
                     FunctionApplication (VarRef idIdentity)
                                         [IntLiteral 86]))
                  @?= 86) :
        [])

testScope :: Scope
testScope = scopeDefault
            |@+| ("ret42", FunctionDefinition [
                              InternalFunction (FunctionType [] rtKindInt) "ret42"
                           ])
            |@+| ("ret43", FunctionDefinition [
                              AFunctionInstance
                                (FunctionType [] rtKindInt)
                                []
                                (AExpression saKindInt $ AIntLiteral (eaKindInt) 43)
                           ])
            |@+| ("identity", FunctionDefinition [
                                 AFunctionInstance
                                   fnIntInt ["a"]
                                   (AExpression saKindInt $
                                                AVarRef eaKindInt (UnqualifiedID "a"))
                              ])
              
ifc :: InternalFunctions
ifc = Map.fromList [("ret42", const $ makeKindInt 42)]
      
idRet42 :: NSID
idRet42 = listToNSID ["ret42"]
idRet43 :: NSID
idRet43 = listToNSID ["ret43"]       
idIdentity :: NSID
idIdentity = listToNSID ["identity"]
