{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module AnalysisTests.Catalogue (catalogueTests) where

import Debug.Trace
import Control.Monad.Except
import Test.Tasty
import Test.Tasty.HUnit
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.Catalogue
import KindLang.Data.KStat
import KindLang.Analysis.BuildCatalogue
import qualified Data.Map as Map
import Data.Map ((!))

nullLoader :: ModuleLoader s
nullLoader sid = throwError $ InvalidImport sid "Could not find module"

buildAndLookupM :: ModuleLoader s -> Module -> Visibility -> NSID ->
                   Bool -> KStat s IdentDefinition
buildAndLookupM l m v s errMode = do
    cats <- buildCatalogues l m
    cat <- catalogueForVisibility v cats
    catchError
      (lookupHierarchical cat s)
      (optionallyAnnotateError errMode cat)
    where
      optionallyAnnotateError True cat e = do
         flattened <- catFlatten cat
         throwError $ InternalError $
                        (show e ++ " -- " ++ show s
                                ++ " -- " ++ show (flatRid <$> flattened))
      optionallyAnnotateError False _ e = throwError e

buildAndLookup :: (forall s . ModuleLoader s) -> Module -> Visibility -> String ->
                  IdentDefinition
buildAndLookup l m v s =
    case buildAndLookupNS l m v (UnqualifiedID s) True of
         Left err -> error $ "Unexpected error " ++ show err
         Right def -> def

buildAndLookupNS :: (forall s . ModuleLoader s) -> Module -> Visibility -> NSID ->
                    Bool -> Either KindError IdentDefinition
buildAndLookupNS l m v s errMode = runToEither $ buildAndLookupM l m v s errMode

catalogueForVisibility :: Visibility -> ModuleCatalogues s -> KStat s (Catalogue s)
catalogueForVisibility Public c = return $ moduleCataloguePublic c
catalogueForVisibility Private c = return $ moduleCataloguePrivate c
catalogueForVisibility Protected _ =
    throwError $ InternalError "modules don't have protected catalogues"

buildToFlatList :: (forall s . ModuleLoader s) -> Module -> Visibility ->
                   [(NSID, NSID, Definition)]
buildToFlatList l m v = expectNoErrors "unexpected error" $
                          buildCatalogues l m >>= catalogueForVisibility v
                                              >>= catFlatten

flatListLookup :: [(NSID, NSID, Definition)] -> NSID -> Maybe (NSID, Definition)
flatListLookup l k = lookup k $ (extractFirst <$> l)

extractFirst :: (a,b,c) -> (a,(b,c))
extractFirst (a,b,c) = (a,(b,c))

buildAndReturnError :: (forall s . ModuleLoader s) -> Module -> Maybe KindError
buildAndReturnError l m = expectNoErrors "unexpected error" $
                            catchError (buildCatalogues l m >> return Nothing)
                                       (\ e -> return $ Just e)
catalogueTests :: TestTree
catalogueTests =
    testGroup "Module catalogues"
    [
        testCase "Empty module produces empty public catalogue" $
                 (buildToFlatList nullLoader (Module Nothing [] []) Public) @?= [],
        testCase "Empty module produces empty private catalogue" $
                 (buildToFlatList nullLoader (Module Nothing [] []) Private) @?= [],
        testCase "Catalogue contains module class definition" $
                 (buildAndLookup
                   nullLoader
                   (Module Nothing []
                        [("MyClass", ClassDefinition[])])
                   Public "MyClass") @?=
                 (UnqualifiedID "MyClass", ClassDefinition []),
        testCase "Catalogue contains module function definition" $
                 (buildAndLookup
                   nullLoader
                   (Module Nothing []
                        [("myFunction", FunctionDefinition [])])
                   Public "myFunction") @?=
                 (UnqualifiedID "myFunction", FunctionDefinition []),
        testCase "Catalogue items qualified when module has name" $
                 (buildAndLookup nullLoader myModule Public "MyClass") @?=
                 (myClassSID, ClassDefinition []),
        testCase "Error importing unknown module" $
                 (buildAndReturnError nullLoader myModuleWithImports) @?=
                 (Just $ InvalidImport myModuleId "Could not find module"),
        testCase "Imported module items in private list" $
                 (buildAndLookup
                    (loaderForModule myModuleId myModule)
                    myModuleWithImports Private "MyClass") @?=
                 (myClassSID, ClassDefinition []),
        testCase "Imported module with filter contains included item" $
                 (buildAndLookup
                    (loaderForModule myModuleId myModule)
                    myModuleWithFilteredImports Private "MyClass") @?=
                 (myClassSID, ClassDefinition []),
        testCase "Item filtered from import not in map" $
                 (flatListLookup (buildToFlatList
                                    (loaderForModule myModuleId myModule)
                                    myModuleWithFilteredImports Private)
                                 (UnqualifiedID "MyOtherClass"))
                 @?= Nothing,
        testCase "Import qualified" $
                 (buildAndLookupNS
                    (loaderForModule myModuleId myModule)
                    myModuleWithQualifiedImports
                    Private myClassSID True) @?=
                 Right (myClassSID, ClassDefinition []),
        testCase "Import qualified with renaming" $
                 (buildAndLookupNS
                    (loaderForModule myModuleId myModule)
                    myModuleWithRenamedImports
                    Private (QualifiedID "I" $ UnqualifiedID "MyClass") True)
                 @?= Right (myClassSID, ClassDefinition []),
        testCase "Qualified filtered imports includes requested item" $
                 (buildAndLookupNS
                    (loaderForModule myModuleId myModule)
                    myModuleWithQualifiedFilteredImports
                    Private myClassSID True) @?=
                 Right (myClassSID, ClassDefinition []),
        testCase "Qualified filtered imports excludes unrequested item" $
                 (buildAndLookupNS
                    (loaderForModule myModuleId myModule)
                    myModuleWithQualifiedFilteredImports
                    Private myOtherClassSID False) @?=
                 Left (IdentifierNotFound myOtherClassSID)
    ]

myModuleId :: NSID
myModuleId = QualifiedID "My" $ UnqualifiedID "Module"

myModule :: Module
myModule = Module (Just myModuleId) []
           [("MyClass", ClassDefinition[]),
            ("MyOtherClass", ClassDefinition[])]

myClassSID :: NSID
myClassSID = QualifiedID "My" $ QualifiedID "Module" $ UnqualifiedID "MyClass"

myOtherClassSID :: NSID
myOtherClassSID = QualifiedID "My" $ QualifiedID "Module" $
                  UnqualifiedID "MyOtherClass"

-- nb this definition does not break circular dependencies!
loaderForModule :: NSID -> Module -> ModuleLoader s
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
