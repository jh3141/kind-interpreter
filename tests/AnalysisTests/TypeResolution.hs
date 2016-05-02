module AnalysisTests.TypeResolution (typeResolutionTests) where

import Control.Monad.Except
import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Analysis.ResolveTypes
import KindLang.Data.Catalogue
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.Scope
import KindLang.Lib.CoreTypes
import KindLang.Util.Control
    
-- fixme self-resolution of catalogues with references between themselves (if
-- this is even necessary?)
typeResolutionTests :: TestTree
typeResolutionTests =
    testGroup "Type resolution"
    [
        testCase "resolve module-level variable with identified type" $
                 (runExcept $ resolveDefListTypes testScope
                  [("v", (VariableDefinition
                          (SimpleType simpleClass)
                          VarInitNone))]) @?=
                 (Right [("v", (VariableDefinition 
                         (ResolvedType simpleClass simpleClass def)
                         VarInitNone))]),
        testCase "resolve variable with renamed type" $
                 (runExcept $ resolveDefListTypes testScope
                  [("v", (VariableDefinition
                          (SimpleType renamedClass)
                          VarInitNone))]) @?=
                 (Right [("v", (VariableDefinition
                         (ResolvedType renamedClass originalClass def)
                         VarInitNone))]),
        testCase "resolve class member variables" $
                 (runExcept $ resolveDefListTypes testScope
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
                 (runExcept $ resolveExpr testScope $ VarRef simpleVar) @?=
                 (Right $ AVarRef (ExprAnnotation
                           (Reference rtSimpleClass)
                           [("CanonicalID", EADId simpleVar)]) simpleVar),
        testCase "resolve int literals" $
                 (runExcept $ resolveExpr testScope $ IntLiteral 1) @?=
                 (Right $ AIntLiteral
                            (ExprAnnotation rtKindInt [])
                            1),
        testCase "resolve string literals" $
                 (runExcept $ resolveExpr testScope $ StringLiteral "foo") @?=
                 (Right $ AStringLiteral
                            (ExprAnnotation rtKindString [])
                            "foo"),
        testCase "pre-resolved expressions returned" $
                 (runExcept $ resolveExpr testScope
                                  (Annotated $ AIntLiteral eaKindInt 42)) @?=
                 (Right $ AIntLiteral eaKindInt 42),
        testCase "resolve object reference" $
                 (runExcept $ resolveExpr testScope $
                              ORef (VarRef ccInst) (UnqualifiedID "v")) @?=
                 (Right $ AORef
                  (ExprAnnotation (Reference rtSimpleClass)
                   [("CanonicalID", EADId $ listToNSID ["ComplexClass","v"])])
                  (AVarRef (ExprAnnotation (Reference rtComplexClass)
                                           [("CanonicalID", EADId ccInst)])
                   ccInst) (UnqualifiedID "v")),
        testCase "resolve function call" $
                 (runExcept $ resolveExpr testScope $
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
                 (runExcept $ resolveExpr testScope $
                              BinOp "+" (IntLiteral 1) (IntLiteral 2)) @?=
                 (Right $ AFunctionApplication eaKindInt
                  (AInternalRef (ExprAnnotation fnIntIntInt []) (coreId "(+)"))
                    -- fixme overloads
                  [(AIntLiteral eaKindInt 1), (AIntLiteral eaKindInt 2)]),

        testCase "resolve prefix operator" $
                 (runExcept $ resolveExpr testScope $
                              PrefixOp "-" (IntLiteral 42)) @?=
                 (Right $ AFunctionApplication eaKindInt
                  (AInternalRef (ExprAnnotation fnIntInt []) (coreId "(u-)"))
                   -- fixme overloads
                  [(AIntLiteral eaKindInt 42)]),

        testCase "resolve object method call" $
                 (runExcept $ resolveExpr testScope $
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
                 (runExcept $ resolveExpr testScope $
                              ORef (VarRef mcInst) privateField) @?=
                 (Left $ AccessViolation
                           (privateField `qualifiedBy` methodClass)
                           Private),

        testCase "variable types inferred" $
                 (runExcept $ resolveDefinition testScope $
                  VariableDefinition InferableType
                                     (VarInitExpr $ IntLiteral 5)) @?=
                 (Right $ VariableDefinition rtKindInt
                          (VarInitAExpr $ AIntLiteral eaKindInt 5)),

        testCase "expression statement resolved" $
                 (runExcept $ resolveStatement testScope $
                                   Expression (IntLiteral 5)) @?=
                 (Right $ AExpression
                            (StmtAnnotation (Just rtKindInt) [] [])
                            (AIntLiteral eaKindInt 5)),

        testCase "variable definition statement resolved" $
                 (runExcept $ resolveStatement testScope $
                  VarDeclStatement "myvar" rtKindInt VarInitNone) @?=
                 (Right $ AVarDeclStatement
                  (StmtAnnotation Nothing
                   [("myvar", VariableDefinition rtKindInt VarInitNone)] [])
                  "myvar" rtKindInt VarInitNone),

        testCase "variable definition with implicit type statement resolved" $
                 (runExcept $ resolveStatement testScope $
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
                 (runExcept $ resolveStatement testScope $
                  StatementBlock
                  [Expression $ StringLiteral "hello",
                   Expression $ IntLiteral 42]) @?=
                 (Right $
                  AStatementBlock saKindInt
                  [AExpression saKindString $ AStringLiteral eaKindString "hello",
                   AExpression saKindInt $ AIntLiteral eaKindInt 42]),
                             
        testCase "statement blocks propagate scope changes" $
                 (runExcept $ resolveStatement testScope $
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
                 (runExcept $ resolveInstance testScope simpleFnInstance) @?=
                 (Right $ simpleFnInstanceResolved  ),

        testCase "resolve function call with type variables in type" $
                 (runExcept $ aexprType <$>
                  (resolveExpr testScope $ FunctionApplication
                                 (VarRef typeVarFn) [(VarRef scInst)])) @?=
                 (Right rtSimpleClass),

        testCase "resolve function call parameters" $
                 (runExcept $ resolveInstance testScope
                  (FunctionInstance fnIntInt ["a"] $
                                    Expression $ VarRef $ UnqualifiedID "a")) @?=
                 (Right $ AFunctionInstance fnIntInt ["a"] $
                          AExpression sarefKindInt $
                           AVarRef (ExprAnnotation (Reference rtKindInt)
                                    [("CanonicalID", EADId $ UnqualifiedID "a")])
                                   (UnqualifiedID "a")),
        testCase "resolve function definition resolves instances" $
                 (runExcept $ resolveDefinition testScope
                            (FunctionDefinition [simpleFnInstance])) @?=
                 (Right $ FunctionDefinition [simpleFnInstanceResolved]),
        testCase "resolve function instance canonicalises explicit types" $
                 (runExcept $ resolveInstance testScope
                            (FunctionInstance
                             (FunctionType [SimpleType $ UnqualifiedID "int"]
                                           (SimpleType $ UnqualifiedID "int"))
                             ["a"]
                             (Expression $ IntLiteral 1))) @?=
                 (Right $ AFunctionInstance fnIntInt ["a"]
                            (AExpression saKindInt $ AIntLiteral eaKindInt 1))
                                          
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
         
testCatalogue :: Catalogue
testCatalogue =
    coreTypes |+| (simpleClass, ClassDefinition [])
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

testScope :: Scope
testScope = Scope Nothing testCatalogue
            
def :: Definition
def = (ClassDefinition [])       
rtSimpleClass :: TypeDescriptor
rtSimpleClass = ResolvedType simpleClass simpleClass def
rtComplexClass :: TypeDescriptor
rtComplexClass = expectNoErrors "internal error" $
                 resolveType testCatalogue complexClass
rtMethodClass :: TypeDescriptor
rtMethodClass = expectNoErrors "internal error" $
                resolveType testCatalogue methodClass
                            
simpleFnInstance :: FunctionInstance
simpleFnInstance = FunctionInstance
                     (FunctionType [rtSimpleClass] rtComplexClass)
                     ["a"]
                     (Expression $ VarRef ccInst)

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
               
