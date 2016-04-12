module AnalysisTests.TypeResolution (typeResolutionTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Analysis.ResolveTypes
import KindLang.Data.Catalogue
import KindLang.Data.BasicTypes
import KindLang.Lib.CoreTypes
    
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
                 [("v", (VariableDefinition 
                         (ResolvedType simpleClass simpleClass def)
                         VarInitNone))],
        testCase "resolve variable with renamed type" $
                 (resolveDefListTypes testCatalogue
                  [("v", (VariableDefinition
                          (SimpleType renamedClass)
                          VarInitNone))]) @?=
                 [("v", (VariableDefinition
                         (ResolvedType renamedClass originalClass def)
                         VarInitNone))],
        testCase "resolve class member variables" $
                 (resolveDefListTypes testCatalogue
                  [("C", (ClassDefinition [
                           (ClassMember "v" Public
                            (VariableDefinition (SimpleType simpleClass)
                                                VarInitNone))]))]) @?=
                 [("C", (ClassDefinition [
                          (ClassMember "v" Public
                           (VariableDefinition
                            rtSimpleClass
                            VarInitNone))]))],
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
                   [("CanonicalID", EADId $ listToScopedID ["ComplexClass","v"])])
                  (AVarRef (ExprAnnotation rtComplexClass
                                           [("CanonicalID", EADId ccInst)])
                   ccInst) (UnqualifiedID "v"))
    ]

simpleClass :: ScopedID
simpleClass = listToScopedID ["SimpleClass"]
complexClass :: ScopedID
complexClass = listToScopedID ["ComplexClass"]
ccInst :: ScopedID
ccInst = listToScopedID ["complexClass"]
simpleFn :: ScopedID
simpleFn = listToScopedID ["simpleFn"]
qualifiedClass :: ScopedID
qualifiedClass = listToScopedID ["package", "module", "QualifiedClass" ]
renamedClass :: ScopedID
renamedClass = listToScopedID ["renamed", "RenamedClass" ]
originalClass :: ScopedID
originalClass = listToScopedID ["original", "RenamedClass" ]
simpleVar :: ScopedID
simpleVar = listToScopedID ["simpleVar"]
         
testCatalogue :: Catalogue
testCatalogue =
    coreTypes |+| (simpleClass, ClassDefinition [])
              |+| (complexClass,
                   ClassDefinition
                     [ClassMember "v" Public
                                  (VariableDefinition rtSimpleClass VarInitNone)])
              |+| (ccInst, VariableDefinition rtComplexClass VarInitNone)
              |+| (simpleFn, FunctionDefinition [])
              |+| (qualifiedClass, ClassDefinition [])
              |++| (renamedClass, originalClass, ClassDefinition [])
              |+| (simpleVar, VariableDefinition rtSimpleClass VarInitNone)

                   
def :: Definition
def = (ClassDefinition [])       
rtSimpleClass :: TypeDescriptor
rtSimpleClass = ResolvedType simpleClass simpleClass def
rtComplexClass :: TypeDescriptor
rtComplexClass = either (error "internal error") id $
                 resolveType testCatalogue complexClass
