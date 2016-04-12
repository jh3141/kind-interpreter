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
                            (ResolvedType simpleClass simpleClass def)
                            VarInitNone))]))],
        testCase "resolve variable reference expressions" $
                 (resolveExpr testCatalogue $ VarRef simpleVar) @?=
                 (Right $ AVarRef (ExprAnnotation
                           (ResolvedType simpleClass simpleClass def)
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
                            "foo")
                                       
    ]

simpleClass :: ScopedID
simpleClass = listToScopedID ["SimpleClass"]
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
              |+| (simpleFn, FunctionDefinition [])
              |+| (qualifiedClass, ClassDefinition [])
              |++| (renamedClass, originalClass, ClassDefinition [])
              |+| (simpleVar, VariableDefinition
                                (ResolvedType simpleClass simpleClass def)
                                VarInitNone)

                   
def :: Definition
def = (ClassDefinition [])       
