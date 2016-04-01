module AnalysisTests.Catalogue (catalogueTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Analysis.BuildCatalogue
import qualified Data.Map as Map
    
nullLoader :: ModuleLoader
nullLoader = const Nothing
                
catalogueTests :: TestTree
catalogueTests =
    testGroup "Module catalogues"
    [
        testCase "Empty module produces empty catalogue" $
                 buildCatalogues nullLoader (Module Nothing [] []) @?=
                 Right (ModuleCatalogues Map.empty Map.empty)
        
    ]
