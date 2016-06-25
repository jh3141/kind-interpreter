{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module ExecutionTests.SimpleEvaluation (simpleEvaluationTests) where

import Control.Monad.Except
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Scope
import KindLang.Data.Value
import KindLang.Data.Catalogue
import KindLang.Lib.CoreTypes
import KindLang.Lib.InternalFunctions
import KindLang.Runtime.Eval
import KindLang.Analysis.ResolveTypes
import KindLang.Data.KStat

-- evaluate expression resolved against scope and extract from error wrapper
execTest :: (forall s . KStat s (Scope s)) -> Expr -> (forall s . Value s)
         -> Assertion
execTest s ex expected = execTestWithData s [] ex expected

execTestWithData :: (forall s . KStat s (Scope s)) ->
                    (forall s . [(NSID,TypeDescriptor,Value s)]) -> Expr ->
                    (forall s . Value s) -> Assertion
execTestWithData s v ex expected =
    either
      (\e -> error (show e)) -- if there's an error
      id                     -- otherwise
      (runToEither executeTest)

    where
      executeTest = do
        s' <- s
        f <- resolveExpr s' ex
        scopeAddItems s' v
        kstatSetInternalFunctions ifc
        val <- evalAExpr s' f >>= refToValue
        return (val @?= expected)

simpleEvaluationTests :: TestTree
simpleEvaluationTests =
    testGroup "Simple evaluation" (
        (testCase "Evaluate an integer literal" $
                  (execTest testScope (IntLiteral 13) (makeKindInt 13))) :
        (testCase "Evaluate no-args internal function" $
                  (execTest testScope
                            (FunctionApplication (VarRef idRet42) [])
                            (makeKindInt 42))) :
        (testCase "Evaluate function with kind definition" $
                  (execTest testScope 
                            (FunctionApplication (VarRef idRet43) [])
                            (makeKindInt 43))) :
        (testCase "Evaluate variable reference" $
                  (execTestWithData testScope
                                    [(idVar1, rtKindInt, makeKindInt 99)]
                                    (VarRef idVar1)
                                    (makeKindInt 99))) :
        (testCase "Evaluate function with arguments" $
                  (execTest testScope 
                            (FunctionApplication (VarRef idIdentity)
                                                 [IntLiteral 86])
                            (makeKindInt 86))) :
        (testCase "Variable definition statement" $
                  (runToEither $ do
                    newScope <- scopeDefault
                    evalAStatement
                                newScope
                                (AVarDeclStatement
                                 (StmtAnnotation Nothing
                                                 [("d",VariableDefinition
                                                         rtKindInt VarInitNone)] [])
                                 "d" rtKindInt VarInitNone)
                    ref <- scopeLookupRef newScope (UnqualifiedID "d") undefined
                    return ()) @?= Right ()) :
        [])

testScope :: KStat s (Scope s)
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
                                   (AExpression saKindInt $ AVarRef
                                    (ExprAnnotation rtKindInt
                                                    [("CanonicalID",EADId $ idA)])
                                    idA)
                              ])
            |@+| ("var1", VariableDefinition rtKindInt VarInitNone)

ifc :: InternalFunctions KStat s
ifc = Map.fromList [("ret42", const $ return $ Left $ makeKindInt 42)]

idRet42 :: NSID
idRet42 = listToNSID ["ret42"]
idRet43 :: NSID
idRet43 = listToNSID ["ret43"]
idIdentity :: NSID
idIdentity = listToNSID ["identity"]
idVar1 :: NSID
idVar1 = listToNSID ["var1"]
idA :: NSID
idA = listToNSID ["a"]
