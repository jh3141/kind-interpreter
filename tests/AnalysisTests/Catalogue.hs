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
                 (UnqualifiedID "MyClass", ClassDefinition [])
                                                  
        
    ]
