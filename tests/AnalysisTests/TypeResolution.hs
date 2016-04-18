module AnalysisTests.TypeResolution (typeResolutionTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Analysis.ResolveTypes
import KindLang.Data.Catalogue
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Lib.CoreTypes
import KindLang.Util.Control
    
-- fixme self-resolution of catalogues with references between themselves (if
-- this is even necessary?)
typeResolutionTests :: TestTree
typeResolutionTests =
    testGroup "Type resolution"
    [
        testCase "resolve module-level variable with identified type" $
                 (resolveDefListTypes testCatalogue
                  [("v", (VariableDefinition
                          (SimpleType simpleClass)
                          VarInitNone))]) @?=
                 (Right [("v", (VariableDefinition 
                         (ResolvedType simpleClass simpleClass def)
                         VarInitNone))]),
        testCase "resolve variable with renamed type" $
                 (resolveDefListTypes testCatalogue
                  [("v", (VariableDefinition
                          (SimpleType renamedClass)
                          VarInitNone))]) @?=
                 (Right [("v", (VariableDefinition
                         (ResolvedType renamedClass originalClass def)
                         VarInitNone))]),
        testCase "resolve class member variables" $
                 (resolveDefListTypes testCatalogue
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
                 (resolveExpr testCatalogue $ VarRef simpleVar) @?=
                 (Right $ AVarRef (ExprAnnotation
                           rtSimpleClass
                           [("CanonicalID", EADId simpleVar)]) simpleVar),
        testCase "resolve int literals" $
                 (resolveExpr testCatalogue $ IntLiteral 1) @?=
                 (Right $ AIntLiteral
                            (ExprAnnotation rtKindInt [])
                            1),
        testCase "resolve string literals" $
                 (resolveExpr testCatalogue $ StringLiteral "foo") @?=
                 (Right $ AStringLiteral
                            (ExprAnnotation rtKindString [])
                            "foo"),
        testCase "pre-resolved expressions returned" $
                 (resolveExpr testCatalogue
                                  (Annotated $ AIntLiteral eaKindInt 42)) @?=
                 (Right $ AIntLiteral eaKindInt 42),
        testCase "resolve object reference" $
                 (resolveExpr testCatalogue $
                              ORef (VarRef ccInst) (UnqualifiedID "v")) @?=
                 (Right $ AORef
                  (ExprAnnotation rtSimpleClass
                   [("CanonicalID", EADId $ listToNSID ["ComplexClass","v"])])
                  (AVarRef (ExprAnnotation rtComplexClass
                                           [("CanonicalID", EADId ccInst)])
                   ccInst) (UnqualifiedID "v")),
        testCase "resolve function call" $
                 (resolveExpr testCatalogue $
                              FunctionApplication
                                (VarRef simpleFn)
                                [(VarRef scInst)]) @?=
                 (Right $ AFunctionApplication
                  (ExprAnnotation rtComplexClass [])
                  (AVarRef
                   (ExprAnnotation tdSimpleFn [("CanonicalID", EADId simpleFn)])
                   simpleFn)
                  [(AVarRef
                    (ExprAnnotation rtSimpleClass
                                    [("CanonicalID", EADId scInst)]) scInst)]),
        testCase "resolve binary operator" $
                 (resolveExpr testCatalogue $
                              BinOp "+" (IntLiteral 1) (IntLiteral 2)) @?=
                 (Right $ AFunctionApplication eaKindInt
                  (AInternalRef (ExprAnnotation fnIntIntInt []) (coreId "(+)"))
                    -- fixme overloads
                  [(AIntLiteral eaKindInt 1), (AIntLiteral eaKindInt 2)]),

        testCase "resolve prefix operator" $
                 (resolveExpr testCatalogue $
                              PrefixOp "-" (IntLiteral 42)) @?=
                 (Right $ AFunctionApplication eaKindInt
                  (AInternalRef (ExprAnnotation fnIntInt []) (coreId "(u-)"))
                   -- fixme overloads
                  [(AIntLiteral eaKindInt 42)]),

        testCase "resolve object method call" $
                 (resolveExpr testCatalogue $
                              OMethod (VarRef mcInst)
                                      method
                                      [VarRef scInst]) @?=
                 (Right $ AOMethod
                  (ExprAnnotation rtComplexClass [])
                  (AVarRef
                   (ExprAnnotation rtMethodClass [("CanonicalID", EADId mcInst)])
                   mcInst)
                  tdSimpleFn
                  (method `qualifiedBy` methodClass)
                  [(AVarRef
                    (ExprAnnotation rtSimpleClass [("CanonicalID", EADId scInst)])
                    scInst)]),

        testCase "attempt to access private member of object" $
                 (resolveExpr testCatalogue $
                              ORef (VarRef mcInst) privateField) @?=
                 (Left $ AccessViolation
                           (privateField `qualifiedBy` methodClass)
                           Private),

        testCase "variable types inferred" $
                 (resolveDefinition testCatalogue $
                  VariableDefinition InferableType
                                     (VarInitExpr $ IntLiteral 5)) @?=
                 (Right $ VariableDefinition rtKindInt
                          (VarInitAExpr $ AIntLiteral eaKindInt 5)),

        testCase "expression statement resolved" $
                 (resolveStatement testCatalogue $
                                   Expression (IntLiteral 5)) @?=
                 (Right $ AExpression
                            (StmtAnnotation (Just rtKindInt) [] [])
                            (AIntLiteral eaKindInt 5)),

        testCase "variable definition statement resolved" $
                 (resolveStatement testCatalogue $
                  VarDeclStatement "myvar" rtKindInt VarInitNone) @?=
                 (Right $ AVarDeclStatement
                           (StmtAnnotation Nothing [("myvar",rtKindInt)] [])
                           "myvar" rtKindInt VarInitNone),

        testCase "variable definition with implicit type statement resolved" $
                 (resolveStatement testCatalogue $
                  VarDeclStatement "myvar" InferableType
                  (VarInitExpr (IntLiteral 5))) @?=
                 (Right $ AVarDeclStatement
                           (StmtAnnotation Nothing [("myvar",rtKindInt)] [])
                           "myvar" rtKindInt
                           (VarInitAExpr (AIntLiteral eaKindInt 5)))
                 
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

                   
def :: Definition
def = (ClassDefinition [])       
rtSimpleClass :: TypeDescriptor
rtSimpleClass = ResolvedType simpleClass simpleClass def
rtComplexClass :: TypeDescriptor
rtComplexClass = rightOrFail "internal error" $
                 resolveType testCatalogue complexClass
rtMethodClass :: TypeDescriptor
rtMethodClass = rightOrFail "internal error" $
                resolveType testCatalogue methodClass
                            
simpleFnInstance :: FunctionInstance
simpleFnInstance = FunctionInstance
                     [("a", rtSimpleClass)]
                     rtComplexClass
                     (Expression $ VarRef ccInst)
tdSimpleFn :: TypeDescriptor
tdSimpleFn = FunctionType [rtSimpleClass] rtComplexClass
