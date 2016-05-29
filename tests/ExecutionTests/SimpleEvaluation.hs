{-# LANGUAGE RankNTypes #-}
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
import KindLang.Runtime.Eval
import KindLang.Analysis.ResolveTypes
import KindLang.Data.KStat

-- evaluate expression resolved against scope and extract from error wrapper
execTest :: (forall s . KStat s (Scope s)) -> Expr -> Value
execTest s ex = execTestWithData s [] ex

execTestWithData :: (forall s . KStat s (Scope s)) -> [(NSID,Value)] -> Expr -> Value
execTestWithData s v ex =
    either
      (\e -> error (show e)) -- if there's an error
      id                     -- otherwise
      (runToEither $ do
                         s' <- s
                         f <- resolveExpr s' ex
                         rts <- runtimeScopeAddItems (newRuntimeScope s') v
                         evalAExpr rts ifc f)

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
        (testCase "Evaluate variable reference" $
                  (getKindInt (execTestWithData testScope [(idVar1, makeKindInt 99)] $
                     VarRef idVar1))
                  @?= 99) :
        (testCase "Evaluate function with arguments" $
                  (getKindInt (execTest testScope $
                     FunctionApplication (VarRef idIdentity)
                                         [IntLiteral 86]))
                  @?= 86) :
        (testCase "Variable definition statement" $
                  (runToEither $ do
                    newScope <- newRuntimeScope <$> scopeDefault
                    (s, _) <- evalAStatement
                                newScope
                                ifc
                                (AVarDeclStatement
                                 (StmtAnnotation Nothing
                                                 [("d",VariableDefinition
                                                         rtKindInt VarInitNone)] [])
                                 "d" rtKindInt VarInitNone)
                    ref <- rtsLookupRef s (UnqualifiedID "d")
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

ifc :: InternalFunctions
ifc = Map.fromList [("ret42", const $ makeKindInt 42)]

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
