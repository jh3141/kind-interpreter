module KindLang.Data.Catalogue where

import qualified Data.Map as Map
    
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.AST
    
type ModuleLoader = ScopedID -> KErr Catalogue
type Catalogue = IdentMap Definition

    
-- this implementation has a complexity that grows linearly
-- with the number of ids to retain.  It would be possible to
-- implement it so that the complexity grows more slowly than this,
-- but it is not entirely clear that this is useful.
-- also consider what existing typeclasses could be used instead of
-- a list of strings, in order to allow for the caller to decide what
-- is the most appropriate structure for them.
catalogueWithOnly :: Catalogue -> [String] -> Catalogue
catalogueWithOnly cat identifiers =
    Map.filterWithKey (\k _ -> elem k identifiers) cat
       
lookupHierarchical :: Catalogue -> ScopedID -> KErr IdentDefinition
lookupHierarchical cat sid@(QualifiedID s s') =
    case Map.lookup s cat of
      Nothing -> Left $ IdentifierNotFound sid
      Just (gsid, Namespace cat2) ->
          case lookupHierarchical cat2 s' of
            Left (IdentifierNotFound _) -> Left $ IdentifierNotFound sid
            r -> r
      Just (gsid, _) -> Left $ NotNamespace gsid s'
                        
                                           
