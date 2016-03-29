module KindLang.Analysis.BuildTypeCatalogue where

import KindLang.Data.AST
import KindLang.Data.Error
import qualified Data.Map as Map

type ModuleLoader = ScopedID -> Maybe (TypeCatalogue, ()) -- unit is a placeholder for function/variable definitions
type TypeCatalogue = Map.Map String (ScopedID, Definition)
data ModuleTypes = ModuleTypes {
      moduleTypesPublic :: TypeCatalogue,
      moduleTypesPrivate :: TypeCatalogue
      } deriving (Show, Eq)
                 
buildTypeCatalogue :: ModuleLoader -> Module -> Either KindError (TypeCatalogue,TypeCatalogue)
buildTypeCatalogue loader m = Right (Map.empty, Map.empty)
                      
