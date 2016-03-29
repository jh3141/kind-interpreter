module AnalysisTests.TypeCatalogue (typeCatalogueTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Analysis.BuildTypeCatalogue
import qualified Data.Map as Map
    
nullLoader :: ModuleLoader
nullLoader = const Nothing
                
typeCatalogueTests :: TestTree
typeCatalogueTests =
    testGroup "Type catalogue"
    [
        testCase "Empty module produces empty catalogue" $
                 buildTypeCatalogue nullLoader (Module Nothing [] []) @?=
                 Right (Map.empty,Map.empty)
        
    ]
