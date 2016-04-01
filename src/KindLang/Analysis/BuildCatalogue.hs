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
buildCatalogues loader m = Right $ ModuleCatalogues Map.empty Map.empty
                      
