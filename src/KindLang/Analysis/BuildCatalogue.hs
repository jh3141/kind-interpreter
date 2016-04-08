module KindLang.Analysis.BuildCatalogue where

import Data.Maybe
    
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

-- fixme refactor this - there's a lot of duplication here.
importModule :: ModuleLoader -> ModuleImport -> KErr Catalogue
importModule loader (UnqualifiedModuleImport sid True) = loader sid
importModule loader (UnqualifiedModuleImport sid False) = loadItem loader sid
importModule loader (QualifiedModuleImport sid True reqid) =
    either Left (Right . makeNamespace (maybe sid id reqid)) (loader sid)
importModule loader (QualifiedModuleImport sid False reqid) =
    either Left (Right . makeNamespace (maybe (fromJust $ qualifierOf sid) id reqid)) (loadItem loader sid)

-- load a single item from the module identified by its qualified id
loadItem :: ModuleLoader -> ScopedID -> KErr Catalogue                         
loadItem loader sid =
    case qualifierOf sid of
      Just msid -> (`catalogueWithOnly` [unscopedIdOf sid]) <$> loader msid
      Nothing   -> Left $ InvalidImport sid
                  "Filtered import must specify both module and identifier (perhaps you wanted 'module::*'?)"
