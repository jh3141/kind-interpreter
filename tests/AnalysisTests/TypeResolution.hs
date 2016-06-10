module AnalysisTests.TypeResolution (typeResolutionTests) where

import Control.Monad.Except
import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Analysis.ResolveTypes
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.Scope
import KindLang.Lib.CoreTypes
import KindLang.Util.Control
import KindLang.Data.KStat

-- fixme self-resolution of catalogues with references between themselves (if
-- this is even necessary?)
typeResolutionTests :: TestTree
typeResolutionTests =
    testGroup "Type resolution"
    [
        testCase "resolve module-level variable with identified type" $
                 (runToEither $ resolveDefListTypesKS testScope
                  [("v", (VariableDefinition
                          (SimpleType simpleClass)
                          VarInitNone))]) @?=
                 (Right [("v", (VariableDefinition
                         (ResolvedType simpleClass simpleClass def)
                         VarInitNone))]),
        testCase "resolve variable with renamed type" $
                 (runToEither $ resolveDefListTypesKS testScope
                  [("v", (VariableDefinition
                          (SimpleType renamedClass)
                          VarInitNone))]) @?=
                 (Right [("v", (VariableDefinition
                         (ResolvedType renamedClass originalClass def)
                         VarInitNone))]),
        testCase "resolve class member variables" $
                 (runToEither $ resolveDefListTypesKS testScope
                  [("C", (ClassDefinition [
                           (ClassMember "v" Public
                            (VariableDefinition (SimpleType simpleClass)
                                                VarInitNone))]))]) @?=
                 (Right [("C", (ClassDefinition [
                          (ClassMember "v" Public
                           (VariableDefinition
                            rtSimpleClass
                            VarInitNone))]))]),

        testCase "resolve variable reference expressions" $
                 (runToEither $ resolveExprKS testScope $ VarRef simpleVar) @?=
                 (Right $ AVarRef (ExprAnnotation
                           (Reference rtSimpleClass)
                           [("CanonicalID", EADId simpleVar)]) simpleVar),
        testCase "resolve int literals" $
                 (runToEither $ resolveExprKS testScope $ IntLiteral 1) @?=
                 (Right $ AIntLiteral
                            (ExprAnnotation rtKindInt [])
                            1),
        testCase "resolve string literals" $
                 (runToEither $ resolveExprKS testScope $ StringLiteral "foo") @?=
                 (Right $ AStringLiteral
                            (ExprAnnotation rtKindString [])
                            "foo"),
        testCase "pre-resolved expressions returned" $
                 (runToEither $ resolveExprKS testScope
                                  (Annotated $ AIntLiteral eaKindInt 42)) @?=
                 (Right $ AIntLiteral eaKindInt 42),
        testCase "resolve object reference" $
                 (runToEither $ resolveExprKS testScope $
                              ORef (VarRef ccInst) (UnqualifiedID "v")) @?=
                 (Right $ AORef
                  (ExprAnnotation (Reference rtSimpleClass)
                   [("CanonicalID", EADId $ listToNSID ["ComplexClass","v"])])
                  (AVarRef (ExprAnnotation (Reference rtComplexClass)
                                           [("CanonicalID", EADId ccInst)])
                   ccInst) (UnqualifiedID "v")),
        testCase "resolve function call" $
                 (runToEither $ resolveExprKS testScope $
                              FunctionApplication
                                (VarRef simpleFn)
                                [(VarRef scInst)]) @?=
                 (Right $ AFunctionApplication
                  (ExprAnnotation rtComplexClass [])
                  (AVarRef (ExprAnnotation tdSimpleFn
                            [("CanonicalID", EADId simpleFn)]) simpleFn)
                  [(AVarRef
                    (ExprAnnotation (Reference rtSimpleClass)
                                    [("CanonicalID", EADId scInst)]) scInst)]),
        testCase "resolve binary operator" $
                 (runToEither $ resolveExprKS testScope $
                              BinOp "+" (IntLiteral 1) (IntLiteral 2)) @?=
                 (Right $ AFunctionApplication eaKindInt
                  (AInternalRef (ExprAnnotation fnIntIntInt []) (coreId "(+)"))
                    -- fixme overloads
                  [(AIntLiteral eaKindInt 1), (AIntLiteral eaKindInt 2)]),

        testCase "resolve prefix operator" $
                 (runToEither $ resolveExprKS testScope $
                              PrefixOp "-" (IntLiteral 42)) @?=
                 (Right $ AFunctionApplication eaKindInt
                  (AInternalRef (ExprAnnotation fnIntInt []) (coreId "(u-)"))
                   -- fixme overloads
                  [(AIntLiteral eaKindInt 42)]),

        testCase "resolve object method call" $
                 (runToEither $ resolveExprKS testScope $
                              OMethod (VarRef mcInst)
                                      method
                                      [VarRef scInst]) @?=
                 (Right $ AOMethod
                  (ExprAnnotation rtComplexClass [])
                  (AVarRef (ExprAnnotation (Reference rtMethodClass)
                            [("CanonicalID", EADId mcInst)]) mcInst)
                  tdSimpleFn
                  (method `qualifiedBy` methodClass)
                  [(AVarRef
                    (ExprAnnotation (Reference rtSimpleClass)
                     [("CanonicalID", EADId scInst)]) scInst)]),

        testCase "attempt to access private member of object" $
                 (runToEither $ resolveExprKS testScope $
                              ORef (VarRef mcInst) privateField) @?=
                 (Left $ AccessViolation
                           (privateField `qualifiedBy` methodClass)
                           Private),

        testCase "variable types inferred" $
                 (runToEither $ resolveDefinitionKS testScope $
                  VariableDefinition InferableType
                                     (VarInitExpr $ IntLiteral 5)) @?=
                 (Right $ VariableDefinition rtKindInt
                          (VarInitAExpr $ AIntLiteral eaKindInt 5)),

        testCase "expression statement resolved" $
                 (runToEither $ resolveStatementKS testScope $
                                   Expression (IntLiteral 5)) @?=
                 (Right $ AExpression
                            (StmtAnnotation (Just rtKindInt) [] [])
                            (AIntLiteral eaKindInt 5)),

        testCase "variable definition statement resolved" $
                 (runToEither $ resolveStatementKS testScope $
                  VarDeclStatement "myvar" rtKindInt VarInitNone) @?=
                 (Right $ AVarDeclStatement
                  (StmtAnnotation Nothing
                   [("myvar", VariableDefinition rtKindInt VarInitNone)] [])
                  "myvar" rtKindInt VarInitNone),

        testCase "variable definition with implicit type statement resolved" $
                 (runToEither $ resolveStatementKS testScope $
                  VarDeclStatement "myvar" InferableType
                  (VarInitExpr (IntLiteral 5))) @?=
                 (Right $ AVarDeclStatement
                  (StmtAnnotation Nothing
                   [("myvar", VariableDefinition rtKindInt VarInitNone)]
                    -- note that initialiser has been removed, as unimportant
                    -- in this context.
                   [])
                  "myvar" rtKindInt (VarInitAExpr (AIntLiteral eaKindInt 5))),
                 -- but it has appeared here.

        testCase "statement block resolved" $
                 (runToEither $ resolveStatementKS testScope $
                  StatementBlock
                  [Expression $ StringLiteral "hello",
                   Expression $ IntLiteral 42]) @?=
                 (Right $
                  AStatementBlock saKindInt
                  [AExpression saKindString $ AStringLiteral eaKindString "hello",
                   AExpression saKindInt $ AIntLiteral eaKindInt 42]),

        testCase "statement blocks propagate scope changes" $
                 (runToEither $ resolveStatementKS testScope $
                  StatementBlock
                  [VarDeclStatement "myvar" rtKindInt VarInitNone,
                   Expression $ VarRef $ UnqualifiedID "myvar"]) @?=
                 (Right $ AStatementBlock sarefKindInt
                  [AVarDeclStatement (StmtAnnotation Nothing
                    [("myvar", VariableDefinition rtKindInt VarInitNone)] [])
                    "myvar" rtKindInt VarInitNone,
                   AExpression sarefKindInt $ AVarRef
                    (ExprAnnotation (Reference rtKindInt)
                     [("CanonicalID", EADId $ UnqualifiedID "myvar")])
                    (UnqualifiedID "myvar")]),

        testCase "resolve function instance" $
                 (runToEither $ resolveInstanceKS testScope simpleFnInstance) @?=
                 (Right $ simpleFnInstanceResolved  ),

        testCase "resolve function call with type variables in type" $
                 (runToEither $ aexprType <$>
                  (resolveExprKS testScope $ FunctionApplication
                                 (VarRef typeVarFn) [(VarRef scInst)])) @?=
                 (Right rtSimpleClass),

        testCase "resolve function call parameters" $
                 (runToEither $ resolveInstanceKS testScope
                  (FunctionInstance fnIntInt ["a"] $
                                    Expression $ VarRef $ UnqualifiedID "a")) @?=
                 (Right $ AFunctionInstance fnIntInt ["a"] $
                          AExpression sarefKindInt $
                           AVarRef (ExprAnnotation (Reference rtKindInt)
                                    [("CanonicalID", EADId $ UnqualifiedID "a")])
                                   (UnqualifiedID "a")),
        testCase "resolve function implementation resolves instances" $
                 (runToEither $ resolveImplementationKS testScope
                            (FunctionDefinition [simpleFnInstance])) @?=
                 (Right $ FunctionDefinition [simpleFnInstanceResolved]),
        testCase "resolve function instance canonicalises explicit types" $
                 (runToEither $ resolveInstanceKS testScope
                            (FunctionInstance
                             (FunctionType [SimpleType $ UnqualifiedID "int"]
                                           (SimpleType $ UnqualifiedID "int"))
                             ["a"]
                             (Expression $ IntLiteral 1))) @?=
                 (Right $ AFunctionInstance fnIntInt ["a"]
                            (AExpression saKindInt $ AIntLiteral eaKindInt 1)),
        testCase "resolve multi-instance function definition -> tuple type" $
                 (runToEither $ aexprType <$> resolveExprKS testScope
                  (VarRef multiFn)) @?=
                 (Right $ TupleType
                            [FunctionType [rtSimpleClass] rtComplexClass,
                             FunctionType [rtComplexClass] rtSimpleClass])
    ]

simpleClass :: NSID
simpleClass = listToNSID ["SimpleClass"]
complexClass :: NSID
complexClass = listToNSID ["ComplexClass"]
methodClass :: NSID
methodClass = listToNSID ["MethodClass"]
scInst :: NSID
scInst = listToNSID ["simpleObject"]
ccInst :: NSID
ccInst = listToNSID ["complexObject"]
mcInst :: NSID
mcInst = listToNSID ["methodObject"]
simpleFn :: NSID
simpleFn = listToNSID ["simpleFn"]
typeVarFn :: NSID
typeVarFn = listToNSID ["typeVarFn"]
qualifiedClass :: NSID
qualifiedClass = listToNSID ["package", "module", "QualifiedClass" ]
renamedClass :: NSID
renamedClass = listToNSID ["renamed", "RenamedClass" ]
originalClass :: NSID
originalClass = listToNSID ["original", "RenamedClass" ]
simpleVar :: NSID
simpleVar = listToNSID ["simpleVar"]
method :: NSID
method = UnqualifiedID "method"
privateField :: NSID
privateField = UnqualifiedID "privateField"
multiFn :: NSID
multiFn = listToNSID ["multiFn"]
          
testScope :: KStat s (Scope s)
testScope =
    scopeDefault
              |+| (simpleClass, ClassDefinition [])
              |+| (complexClass,
                   ClassDefinition
                     [ClassMember "v" Public
                                  (VariableDefinition rtSimpleClass VarInitNone)])
              |+| (scInst, VariableDefinition rtSimpleClass VarInitNone)
              |+| (ccInst, VariableDefinition rtComplexClass VarInitNone)
              |+| (simpleFn, FunctionDefinition [simpleFnInstance])
              |+| (typeVarFn, FunctionDefinition [typeVarFnInstance])
              |+| (qualifiedClass, ClassDefinition [])
              |++| (renamedClass, originalClass, ClassDefinition [])
              |+| (simpleVar, VariableDefinition rtSimpleClass VarInitNone)
              |+| (methodClass,
                   ClassDefinition
                    [ClassMember "method" Public
                                 (FunctionDefinition [simpleFnInstance]),
                     ClassMember "privateField" Private
                                 (VariableDefinition rtSimpleClass VarInitNone)])
              |+| (mcInst, VariableDefinition rtMethodClass VarInitNone)
              |+| (multiFn, FunctionDefinition [simpleFnInstance,
                                                simpleFnInstance2])

def :: Definition
def = (ClassDefinition [])
rtSimpleClass :: TypeDescriptor
rtSimpleClass = ResolvedType simpleClass simpleClass def
rtComplexClass :: TypeDescriptor
rtComplexClass = expectNoErrors "internal error" $
                 resolveTypeKS testScope complexClass
rtMethodClass :: TypeDescriptor
rtMethodClass = expectNoErrors "internal error" $
                resolveTypeKS testScope methodClass

simpleFnInstance :: FunctionInstance
simpleFnInstance = FunctionInstance
                     (FunctionType [rtSimpleClass] rtComplexClass)
                     ["a"]
                     (Expression $ VarRef ccInst)

simpleFnInstance2 :: FunctionInstance
simpleFnInstance2 = FunctionInstance
                     (FunctionType [rtComplexClass] rtSimpleClass)
                     ["a"]
                     (Expression $ VarRef scInst)

simpleFnInstanceResolved :: FunctionInstance
simpleFnInstanceResolved =
    AFunctionInstance tdSimpleFn ["a"]
                      (AExpression
                       (StmtAnnotation (Just $ Reference rtComplexClass) [] []) $
                       (AVarRef
                        (ExprAnnotation (Reference rtComplexClass)
                                            [("CanonicalID", EADId ccInst)])
                        ccInst))

typeVarFnInstance :: FunctionInstance
typeVarFnInstance =
    FunctionInstance
      (ForAllTypes ["a"] [] $
                   FunctionType [TypeVariable "a"] (TypeVariable "a"))
      ["value"]
      (Expression $ VarRef $ UnqualifiedID "value")

tdSimpleFn :: TypeDescriptor
tdSimpleFn = FunctionType [rtSimpleClass] rtComplexClass
               
