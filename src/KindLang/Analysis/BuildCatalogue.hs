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

-- | A type for functions that are able to load the public catalogue from a
-- module with a specified id.
type ModuleLoader s = NSID -> KStat s (Catalogue s)

-- | Defines the catalogues (i.e. maps of symbols to definitions) produced by a
-- module; one provides the public API of the module, while another provides
-- those definitions that are private to the module (including the public APIs
-- of any modules that are imported to the module).
data ModuleCatalogues s = ModuleCatalogues {
      moduleCataloguePublic :: Catalogue s,
      moduleCataloguePrivate :: Catalogue s
      } deriving (Show, Eq)

-- | Generates the catalogues for a module, using the specified module loader
-- to generate catalogues for any module that is imported to the module.
buildCatalogues :: ModuleLoader s -> Module -> KStat s (ModuleCatalogues s)
buildCatalogues loader m = do
    importedModuleSymbols <- importModules loader (moduleImportList m) Map.empty
    publicSymbols <-
           catalogueForDefinitionList qualifyStringIds $ moduleDeclarationList m
    return $ ModuleCatalogues publicSymbols importedModuleSymbols
    where
      qualifyStringIds string =
          case moduleName m of
            Just mid -> (UnqualifiedID string) `qualifiedBy`mid
            Nothing  -> (UnqualifiedID string)

-- fixme this could be implemented as  a fold, but I'm too lazy to work
-- out how right now.
-- | Loads all modules imported in a list of import statements and adds them
-- to the specified catalogue, returning the updated catalogue or an error.
importModules :: ModuleLoader s -> [ModuleImport] -> Catalogue s ->
                 KStat s (Catalogue s)

importModules _ [] imported = return imported

importModules loader (moduleImport:imports) imported =
    importModule loader moduleImport >>= recurse
    where
      recurse importedDefinitions = importModules loader imports
                                        (Map.union imported importedDefinitions)

-- | Produces the required catalogue of changes for a given module import
-- statement.  Note that this may not be exactly the same as the module's
-- export list, e.g. if a module is only partially imported.
importModule :: ModuleLoader s -> ModuleImport -> KStat s (Catalogue s)
importModule loader (UnqualifiedModuleImport sid True) = loader sid
importModule loader (UnqualifiedModuleImport sid False) = loadItem loader sid
importModule loader (QualifiedModuleImport sid True reqid) =
     loader sid >>= makeNamespace (maybe sid id reqid)
importModule loader (QualifiedModuleImport sid False reqid) =
     loadItem loader sid >>=
              makeNamespace (maybe (fromJust $ qualifierOf sid) id reqid)

-- | load a single item from the module identified by its qualified id
loadItem :: ModuleLoader s -> NSID -> KStat s (Catalogue s)
loadItem loader sid =
    case qualifierOf sid of
      Just msid -> loader msid >>=
                   (`catalogueWithOnly` [withoutNamespace sid])
                   -- fixme the above line is unnecessarily cryptic.
      Nothing   -> throwError $ InvalidImport sid filterRequiresIdentifier

-- | Creates a child of a given parent scope for a module whose catalogues
-- are provided.
makeModuleScope :: Scope s -> ModuleCatalogues s -> Scope s
makeModuleScope p (ModuleCatalogues pub priv) = Scope (Just p) (Map.union pub priv)

buildScope :: ModuleLoader s -> Scope s -> Module -> KStat s (Scope s)
buildScope ldr s m = makeModuleScope s <$> buildCatalogues ldr m

-- | A module loader implementation that fails if any module is requested
nullModuleLoader :: NSID -> KStat s (Catalogue s)
nullModuleLoader _ = throwError $ InternalError "module loading not available"
