module AnalysisTests.TypeResolution (typeResolutionTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Analysis.ResolveTypes
import KindLang.Data.Catalogue
import KindLang.Data.BasicTypes
    
typeResolutionTests :: TestTree
typeResolutionTests =
    testGroup "Type resolution"
    [
        testCase "resolve module-level variable with identified type" $
                 (resolveDeflistTypes testCatalogue
                  [("v", (VariableDefinition
                          (SimpleType simpleClass)
                          VarInitNone))]) @?=
                 [("v", (VariableDefinition 
                         (ResolvedType simpleClass simpleClass def)
                         VarInitNone))],
        testCase "resolve variable with renamed type" $
                 (resolveDeflistTypes testCatalogue
                  [("v", (VariableDefinition
                          (SimpleType renamedClass)
                          VarInitNone))]) @?=
                 [("v", (VariableDefinition
                         (ResolvedType renamedClass originalClass def)
                         VarInitNone))]
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
                 
testCatalogue :: Catalogue
testCatalogue =
    newCatalogue |+| (simpleClass, ClassDefinition [])
                 |+| (simpleFn, FunctionDefinition [])
                 |+| (qualifiedClass, ClassDefinition [])
                 |++| (renamedClass, originalClass, ClassDefinition [])

                   
def :: Definition
def = (ClassDefinition [])       
