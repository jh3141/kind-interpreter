module AnalysisTests.Catalogue (catalogueTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Analysis.BuildCatalogue
import qualified Data.Map as Map
import Data.Map ((!))
    
nullLoader :: ModuleLoader
nullLoader = const Nothing

buildAndGetCat :: ModuleLoader -> Module -> ModuleCatalogues
buildAndGetCat l m = case buildCatalogues l m of
                       Left err -> error $ "Unexpected error " ++ show err
                       Right cat -> cat
                                    
catalogueTests :: TestTree
catalogueTests =
    testGroup "Module catalogues"
    [
        testCase "Empty module produces empty catalogue" $
                 buildCatalogues nullLoader (Module Nothing [] []) @?=
                 Right (ModuleCatalogues Map.empty Map.empty),
        testCase "Catalogue contains module class definition" $
                 (moduleCataloguePublic (buildAndGetCat
                   nullLoader
                   (Module Nothing []
                        [("MyClass", ClassDefinition[])])) ! "MyClass") @?=
                 (UnqualifiedID "MyClass", ClassDefinition []),
        testCase "Catalogue contains module function definition" $
                 (moduleCataloguePublic (buildAndGetCat
                   nullLoader
                   (Module Nothing []
                        [("myFunction", FunctionDefinition [])]))
                  ! "myFunction") @?=
                 (UnqualifiedID "myFunction", FunctionDefinition []),
        testCase "Catalogue items qualified when module has name" $
                 (moduleCataloguePublic (buildAndGetCat
                   nullLoader
                   (Module (Just $ QualifiedID "My" $ UnqualifiedID "Module")
                        []
                        [("MyClass", ClassDefinition[])])) ! "MyClass") @?=
                 (QualifiedID "My" $ QualifiedID "Module" $
                              UnqualifiedID "MyClass", ClassDefinition [])
        
                                                  
        
    ]
