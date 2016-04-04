module KindLang.Analysis.BuildCatalogue where

import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.Catalogue
import qualified Data.Map as Map

data ModuleCatalogues = ModuleCatalogues {
      moduleCataloguePublic :: Catalogue,
      moduleCataloguePrivate :: Catalogue
      } deriving (Show, Eq)
                 
buildCatalogues :: ModuleLoader -> Module -> KErr ModuleCatalogues
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
                 KErr Catalogue
                        
importModules _ [] imported =
    Right imported
          
importModules loader (moduleImport:imports) imported =
    case importModule loader moduleImport of
      Left err -> Left err
      Right importedDefinitions ->
          importModules loader imports
                        (Map.union imported importedDefinitions)

importModule :: ModuleLoader -> ModuleImport -> KErr Catalogue
importModule loader (UnqualifiedModuleImport sid True) = loader sid
importModule loader (UnqualifiedModuleImport sid False) =
    case qualifierOf sid of
      Just msid -> (`catalogueWithOnly` [unscopedIdOf sid]) <$> loader msid
      Nothing   -> Left $ InvalidImport sid
                  "Filtered import must specify both module and identifier (perhaps you wanted 'module::*'?)"
      
importModule _ imp =
    Left $ InvalidImport (UnqualifiedID "*")
                         ("Unimplemented import type " ++ (show imp))
         

