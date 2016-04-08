module AnalysisTests.Catalogue (catalogueTests) where

import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.Catalogue
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
                 Nothing,
        testCase "Import qualified" $
                 (lookupHierarchical
                  (moduleCataloguePrivate
                   (buildAndGetCat
                    (loaderForModule myModuleId myModule)
                    myModuleWithQualifiedImports))
                  myClassSID) @?= Right (myClassSID, ClassDefinition []),
        testCase "Import qualified with renaming" $
                 (lookupHierarchical
                  (moduleCataloguePrivate
                   (buildAndGetCat
                    (loaderForModule myModuleId myModule)
                    myModuleWithRenamedImports))
                  (QualifiedID "I" $ UnqualifiedID "MyClass"))
                 @?= Right (myClassSID, ClassDefinition []),
        testCase "Qualified filtered imports includes requested item" $
                 (lookupHierarchical
                  (moduleCataloguePrivate
                   (buildAndGetCat
                    (loaderForModule myModuleId myModule)
                    myModuleWithQualifiedFilteredImports))
                  myClassSID) @?= Right (myClassSID, ClassDefinition []),
        testCase "Qualified filtered imports excludes unrequested item" $
                 (lookupHierarchical
                  (moduleCataloguePrivate
                   (buildAndGetCat
                    (loaderForModule myModuleId myModule)
                    myModuleWithQualifiedFilteredImports))
                  myOtherClassSID) @?=
                 Left (IdentifierNotFound myOtherClassSID)
                 
    ]

myModuleId :: ScopedID
myModuleId = QualifiedID "My" $ UnqualifiedID "Module"
             
myModule :: Module
myModule = Module (Just myModuleId) []
           [("MyClass", ClassDefinition[]),
            ("MyOtherClass", ClassDefinition[])]

myClassSID :: ScopedID
myClassSID = QualifiedID "My" $ QualifiedID "Module" $ UnqualifiedID "MyClass"

myOtherClassSID :: ScopedID
myOtherClassSID = QualifiedID "My" $ QualifiedID "Module" $
                  UnqualifiedID "MyOtherClass"
             
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
           [UnqualifiedModuleImport myClassSID False] []

myModuleWithQualifiedImports :: Module
myModuleWithQualifiedImports =
    Module (Nothing)
           [QualifiedModuleImport myModuleId True Nothing]
           []

myModuleWithRenamedImports :: Module
myModuleWithRenamedImports =
    Module (Nothing)
           [QualifiedModuleImport myModuleId True (Just (UnqualifiedID "I"))]
           []

myModuleWithQualifiedFilteredImports :: Module
myModuleWithQualifiedFilteredImports =
    Module (Nothing)
           [QualifiedModuleImport myClassSID False Nothing]
           []
           
