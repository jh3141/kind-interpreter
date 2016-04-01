module KindLang.Analysis.BuildCatalogue where

import KindLang.Data.AST
import KindLang.Data.Error
import qualified Data.Map as Map

type ModuleLoader = ScopedID -> Maybe Catalogue
type Catalogue = Map.Map String (ScopedID, Definition)
data ModuleCatalogues = ModuleCatalogues {
      moduleCataloguePublic :: Catalogue,
      moduleCataloguePrivate :: Catalogue
      } deriving (Show, Eq)
                 
buildCatalogues :: ModuleLoader -> Module -> Either KindError ModuleCatalogues
buildCatalogues loader m =
    Right $ ModuleCatalogues (addScopedIds definitionMap) Map.empty
    where
      definitionMap = Map.fromList $ moduleDeclarationList m
      addScopedIds = Map.mapWithKey (\k v -> (scopedIdFor k, v)) 
      scopedIdFor string = UnqualifiedID string -- fixme
