module KindLang.Analysis.BuildCatalogue where

import KindLang.Data.AST
import KindLang.Data.Error
import qualified Data.Map as Map

type ModuleLoader = ScopedID -> Either KindError Catalogue
type Catalogue = Map.Map String (ScopedID, Definition)
data ModuleCatalogues = ModuleCatalogues {
      moduleCataloguePublic :: Catalogue,
      moduleCataloguePrivate :: Catalogue
      } deriving (Show, Eq)
                 
buildCatalogues :: ModuleLoader -> Module -> Either KindError ModuleCatalogues
buildCatalogues loader m =
    case importModules loader (moduleImportList m) Map.empty  of
      Left err ->
          Left err
      Right imported ->
          Right $ ModuleCatalogues (addScopedIds definitionMap) (imported)
    where
      definitionMap = Map.fromList $ moduleDeclarationList m
      addScopedIds = Map.mapWithKey (\k v -> (scopedIdFor k, v)) 
      scopedIdFor string =
          case moduleName m of
            Just mid -> (UnqualifiedID string) `qualifiedBy`mid
            Nothing  -> (UnqualifiedID string)


-- fixme this could be implemented as  a fold, but I'm too lazy to work
-- out how right now.
importModules :: ModuleLoader -> [ModuleImport] -> Catalogue ->
                 Either KindError Catalogue
                        
importModules _ [] imported =
    Right imported
          
importModules loader (moduleImport:imports) imported =
    case importModule loader moduleImport of
      Left err -> Left err
      Right importedDefinitions ->
          importModules loader imports
                        (Map.union imported importedDefinitions)

importModule :: ModuleLoader -> ModuleImport ->
                Either KindError Catalogue
importModule loader (UnqualifiedModuleImport sid True) = loader sid
importModule _ imp =
    Left $ InvalidImport (UnqualifiedID "*")
                         ("Unimplemented import type " ++ (show imp))
         
