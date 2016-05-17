module KindLang.Analysis.BuildCatalogue where

import Data.Maybe
import Control.Monad.Except
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.KStat
import KindLang.Locale.ErrorMessages
import KindLang.Data.Catalogue
import KindLang.Data.Scope
import qualified Data.Map as Map

-- | Defines the catalogues (i.e. maps of symbols to definitions) produced by a
-- module; one provides the public API of the module, while another provides
-- those definitions that are private to the module (including the public APIs
-- of any modules that are imported to the module).
data ModuleCatalogues = ModuleCatalogues {
      moduleCataloguePublic :: Catalogue,
      moduleCataloguePrivate :: Catalogue
      } deriving (Show, Eq)

-- | Generates the catalogues for a module, using the specified module loader
-- to generate catalogues for any module that is imported to the module.
buildCatalogues :: ModuleLoader s -> Module -> KStat s ModuleCatalogues
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
-- | Loads all modules imported in a list of import statements and adds them
-- to the specified catalogue, returning the updated catalogue or an error.
importModules :: ModuleLoader s -> [ModuleImport] -> Catalogue ->
                 KStat s Catalogue

importModules _ [] imported = return imported

importModules loader (moduleImport:imports) imported =
    importModule loader moduleImport >>= recurse
    where
      recurse importedDefinitions = importModules loader imports
                                        (Map.union imported importedDefinitions)

-- | Produces the required catalogue of changes for a given module import
-- statement.  Note that this may not be exactly the same as the module's
-- export list, e.g. if a module is only partially imported.
importModule :: ModuleLoader s -> ModuleImport -> KStat s Catalogue
importModule loader (UnqualifiedModuleImport sid True) = loader sid
importModule loader (UnqualifiedModuleImport sid False) = loadItem loader sid
importModule loader (QualifiedModuleImport sid True reqid) =
    makeNamespace (maybe sid id reqid) <$> loader sid
importModule loader (QualifiedModuleImport sid False reqid) =
    makeNamespace (maybe (fromJust $ qualifierOf sid) id reqid) <$> loadItem loader sid

-- | load a single item from the module identified by its qualified id
loadItem :: ModuleLoader s -> NSID -> KStat s Catalogue
loadItem loader sid =
    case qualifierOf sid of
      Just msid -> (`catalogueWithOnly` [withoutNamespace sid]) <$> loader msid
      Nothing   -> throwError $ InvalidImport sid filterRequiresIdentifier

-- | Creates a child of a given parent scope for a module whose catalogues
-- are provided.
makeModuleScope :: Scope -> ModuleCatalogues -> Scope
makeModuleScope p (ModuleCatalogues pub priv) = Scope (Just p) (Map.union pub priv)
