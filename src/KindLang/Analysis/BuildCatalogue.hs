module KindLang.Analysis.BuildCatalogue where

import Data.Maybe
import Control.Monad.Except
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Locale.ErrorMessages
import KindLang.Data.Catalogue
import qualified Data.Map as Map

data ModuleCatalogues = ModuleCatalogues {
      moduleCataloguePublic :: Catalogue,
      moduleCataloguePrivate :: Catalogue
      } deriving (Show, Eq)
                 
buildCatalogues :: ModuleLoader -> Module -> KErr ModuleCatalogues
buildCatalogues loader m =
    ModuleCatalogues (addScopedIds definitionMap) <$>
      importModules loader (moduleImportList m) Map.empty   
    where
      definitionMap = Map.fromList $ moduleDeclarationList m
      addScopedIds = Map.mapWithKey (\k v -> (nsidFor k, v)) 
      nsidFor string =
          case moduleName m of
            Just mid -> (UnqualifiedID string) `qualifiedBy`mid
            Nothing  -> (UnqualifiedID string)


-- fixme this could be implemented as  a fold, but I'm too lazy to work
-- out how right now.
importModules :: ModuleLoader -> [ModuleImport] -> Catalogue ->
                 KErr Catalogue
                        
importModules _ [] imported = return imported
          
importModules loader (moduleImport:imports) imported =
    importModule loader moduleImport >>= recurse
    where
      recurse importedDefinitions = importModules loader imports
                                        (Map.union imported importedDefinitions)

importModule :: ModuleLoader -> ModuleImport -> KErr Catalogue
importModule loader (UnqualifiedModuleImport sid True) = loader sid
importModule loader (UnqualifiedModuleImport sid False) = loadItem loader sid
importModule loader (QualifiedModuleImport sid True reqid) =
    makeNamespace (maybe sid id reqid) <$> loader sid
importModule loader (QualifiedModuleImport sid False reqid) =
    makeNamespace (maybe (fromJust $ qualifierOf sid) id reqid) <$> loadItem loader sid

-- load a single item from the module identified by its qualified id
loadItem :: ModuleLoader -> NSID -> KErr Catalogue                         
loadItem loader sid =
    case qualifierOf sid of
      Just msid -> (`catalogueWithOnly` [withoutNamespace sid]) <$> loader msid
      Nothing   -> throwError $ InvalidImport sid filterRequiresIdentifier
                  
