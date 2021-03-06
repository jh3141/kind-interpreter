module KindLang.Analysis.BuildCatalogue where

import Debug.Trace
import Data.Maybe
import Control.Monad.Except
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.KStat
import KindLang.Locale.ErrorMessages
import KindLang.Data.Catalogue
import KindLang.Data.Scope
import KindLang.Runtime.Data
import qualified Data.Map as Map
    
-- | An alias for catalogues containing values 
type ValueCat s = Catalogue s (Value s)

-- | A type for functions that are able to load the public catalogue from a
-- module with a specified id.
type ModuleLoader s = NSID -> KStat s (ValueCat s)

-- | Defines the catalogues (i.e. maps of symbols to definitions) produced by a
-- module; one provides the public API of the module, while another provides
-- those definitions that are private to the module (including the public APIs
-- of any modules that are imported to the module).
data ModuleCatalogues s = ModuleCatalogues {
      moduleCataloguePublic :: ValueCat s,
      moduleCataloguePrivate :: ValueCat s
      } deriving (Show)

-- | Generates the catalogues for a module, using the specified module loader
-- to generate catalogues for any module that is imported to the module.
buildCatalogues :: ModuleLoader s -> Module -> KStat s (ModuleCatalogues s)
buildCatalogues loader m = do
    newCat <- newCatalogue
    importModules loader (moduleImportList m) newCat
    publicSymbols <-
           catalogueForDefinitionList qualifyStringIds $ moduleDeclarationList m
    return $ ModuleCatalogues publicSymbols newCat
    where
      qualifyStringIds string =
          case moduleName m of
            Just mid -> (UnqualifiedID string) `qualifiedBy` mid
            Nothing  -> (UnqualifiedID string)

-- | Loads all modules imported in a list of import statements and adds them
-- to the specified catalogue, returning () or an error.
importModules :: ModuleLoader s -> [ModuleImport] -> ValueCat s ->
                 KStat s ()

importModules loader imports target =
    mapM_ (\ m -> importModule loader m >>= catalogueCopyTo target) imports

-- | Produces the required catalogue of changes for a given module import
-- statement.  Note that this may not be exactly the same as the module's
-- export list, e.g. if a module is only partially imported.
importModule :: ModuleLoader s -> ModuleImport -> KStat s (ValueCat s)
importModule loader (UnqualifiedModuleImport sid True) = loader sid
importModule loader (UnqualifiedModuleImport sid False) = loadItem loader sid
importModule loader (QualifiedModuleImport sid True reqid) = do
     cat <- loader sid
     makeNamespace (maybe sid id reqid) cat

importModule loader (QualifiedModuleImport sid False reqid) = do
     cat <- loadItem loader sid
     makeNamespace (maybe (fromJust $ qualifierOf sid) id reqid) cat


-- | load a single item from the module identified by its qualified id
loadItem :: ModuleLoader s -> NSID -> KStat s (ValueCat s)
loadItem loader sid =
    case qualifierOf sid of
      Just msid -> loader msid >>=
                   (`catalogueWithOnly` [withoutNamespace sid])
                   -- fixme the above line is unnecessarily cryptic.
      Nothing   -> throwError $ InvalidImport sid filterRequiresIdentifier

-- | Creates a child of a given parent scope for a module whose catalogues
-- are provided, running the specified preprocessing function on the
-- new scope(s)
makeModuleScope :: Scope s -> (Scope s -> KStat s a) -> ModuleCatalogues s -> 
                   KStat s (Scope s)
makeModuleScope p process (ModuleCatalogues pub priv) =
    do
      process privateScope
      process publicScope
      return publicScope
    where
      publicScope  = Scope (Just privateScope) pub
      privateScope = Scope (Just p)            priv

-- | Creates public and private scopes for the specified module,
-- and runs the specified preprocessing function on them, discarding any
-- result.
buildScope :: ModuleLoader s -> Scope s -> Module ->
              (Scope s -> KStat s a) -> KStat s (Scope s)
buildScope ldr s m pre = buildCatalogues ldr m >>=
                         makeModuleScope s pre

-- | A module loader implementation that fails if any module is requested
nullModuleLoader :: NSID -> KStat s (ValueCat s)
nullModuleLoader _ = throwError $ InternalError "module loading not available"
