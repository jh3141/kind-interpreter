{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module ExecutionTests.SimpleEvaluation (simpleEvaluationTests) where

import Control.Monad.Except
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import Test.Rematch.TastyHUnit
import Control.Rematch
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Scope
import KindLang.Data.Value
import KindLang.Data.Catalogue
import KindLang.Lib.CoreTypes
import KindLang.Lib.InternalFunctions
import KindLang.Runtime.Eval
import KindLang.Runtime.Metaclass
import KindLang.Analysis.ResolveTypes
import KindLang.Data.KStat

-- evaluate expression resolved against scope and extract from error wrapper
execTest :: (forall s . KStat s (Scope s)) -> Expr ->
            (forall s . Value s -> KStat s Assertion) ->
            Assertion
execTest s ex assertion = execTestWithData s [] ex assertion

execTestWithData :: (forall s . KStat s (Scope s)) ->
                    (forall s . [(NSID,TypeDescriptor,Value s)]) -> Expr ->
                    (forall s . Value s -> KStat s Assertion) -> Assertion
execTestWithData s v ex assertion =
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
        evalAExpr s' f >>= refToValue >>= assertion


assertEqualM :: Monad m => v -> v -> m Assertion
assertEqualM expected actual = return (actual @?= expected)

simpleEvaluationTests :: TestTree
simpleEvaluationTests =
    testGroup "Simple evaluation" (
        (testCase "Evaluate an integer literal" $
                  (execTest testScope (IntLiteral 13)
                                      (assertEqualM $ makeKindInt 13))) :
        (testCase "Evaluate no-args internal function" $
                  (execTest testScope
                            (FunctionApplication (VarRef idRet42) [])
                            (assertEqualM $ makeKindInt 42))) :
        (testCase "Evaluate function with kind definition" $
                  (execTest testScope 
                            (FunctionApplication (VarRef idRet43) [])
                            (assertEqualM $ makeKindInt 43))) :
        (testCase "Evaluate variable reference" $
                  (execTestWithData testScope
                                    [(idVar1, rtKindInt, makeKindInt 99)]
                                    (VarRef idVar1)
                                    (return $ makeKindInt 99))) :
        (testCase "Evaluate function with arguments" $
                  (execTest testScope 
                            (FunctionApplication (VarRef idIdentity)
                                                 [IntLiteral 86])
                            (assertEqualM $ makeKindInt 86))) :
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
        (testCase "Evaluate reference to a class" $
                  (execTest testScope
                            (VarRef idMyClass) 
                            (\ (KindObject rMetaclass rSlots) -> do
                               defaultMeta <- getKindDefaultMetaclass testScope
                               return $ expect rMetaclass (is defaultMeta))
                  )) :
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
            |@+| ("MyClass", ClassDefinition [])

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
idMyClass :: NSID
idMyClass = listToNSID ["MyClass"]
