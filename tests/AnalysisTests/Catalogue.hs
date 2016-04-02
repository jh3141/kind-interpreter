module AnalysisTests.Catalogue (catalogueTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Analysis.BuildCatalogue
import qualified Data.Map as Map
import Data.Map ((!))
    
nullLoader :: ModuleLoader
nullLoader sid = Left $ InvalidImport sid "Could not find module"

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
                 ((moduleCataloguePublic
                   (buildAndGetCat nullLoader myModule)) ! "MyClass") @?=
                 (myClassSID, ClassDefinition []),
        testCase "Error importing unknown module" $
                 (buildCatalogues nullLoader myModuleWithImports) @?=
                 (Left $ InvalidImport myModuleId "Could not find module"),
        testCase "Imported module items in private list" $
                 ((moduleCataloguePrivate
                   (buildAndGetCat
                    (loaderForModule myModuleId myModule)
                    myModuleWithImports)) ! "MyClass") @?=
                 (myClassSID, ClassDefinition []),
        testCase "Imported module with filter contains included item" $
                 ((moduleCataloguePrivate
                   (buildAndGetCat
                    (loaderForModule myModuleId myModule)
                    myModuleWithFilteredImports)) ! "MyClass") @?=
                 (myClassSID, ClassDefinition []),
        testCase "Item filtered from import not in map" $
                 (Map.lookup "MyOtherClass"
                  (moduleCataloguePrivate
                   (buildAndGetCat
                    (loaderForModule myModuleId myModule)
                    myModuleWithFilteredImports))) @?=
                 Nothing
                                                  
        
    ]

myModuleId :: ScopedID
myModuleId = QualifiedID "My" $ UnqualifiedID "Module"
             
myModule :: Module
myModule = Module (Just myModuleId) []
           [("MyClass", ClassDefinition[]),
            ("MyOtherClass", ClassDefinition[])]

myClassSID :: ScopedID
myClassSID = QualifiedID "My" $ QualifiedID "Module" $ UnqualifiedID "MyClass"
             
-- nb this definition does not break circular dependencies!
loaderForModule :: ScopedID -> Module -> ModuleLoader
loaderForModule sid m sidQ | sid == sidQ
                               = moduleCataloguePublic <$>
                                 buildCatalogues nullLoader m
loaderForModule _ _ sid        = nullLoader sid
                            
myModuleWithImports :: Module
myModuleWithImports = Module (Nothing)
                      [UnqualifiedModuleImport myModuleId True] []
                                                                
myModuleWithFilteredImports :: Module
myModuleWithFilteredImports =
    Module (Nothing)
           [UnqualifiedModuleImport
            ((UnqualifiedID "MyClass") `qualifiedBy` myModuleId)
            False] []
