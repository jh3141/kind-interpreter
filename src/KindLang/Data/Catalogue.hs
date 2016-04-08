module KindLang.Data.Catalogue where

import qualified Data.Map as Map
    
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.AST
    
type ModuleLoader = ScopedID -> KErr Catalogue
type Catalogue = IdentMap Definition


newCatalogue :: Catalogue
newCatalogue = Map.empty

catAdd :: Catalogue -> ScopedID -> Definition -> Catalogue
catAdd cat sid def =
    updatedCat cat sid []
    where
      updatedCat c' (UnqualifiedID s) _ =
          Map.insert s (sid,def) c'
      updatedCat c' (QualifiedID s s') qualifiers | s `Map.notMember` c' =
          Map.insert s (s `qualifiedByStrings` qualifiers,
                        Namespace $ updatedCat newCatalogue s' (s:qualifiers)) c'
      updatedCat c' (QualifiedID s s') qualifiers =
          case Map.lookup s c' of
            Just (nssid, Namespace nscat) ->
                Map.insert s
                       (nssid,
                        Namespace $ updatedCat nscat s' (s:qualifiers))
                       c'
            _ -> error ("attempted to insert item into non-namespace definition "
                        ++ "of catalogue " ++ show sid)

                         
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
      Just (_, Namespace cat2) ->
          case lookupHierarchical cat2 s' of
            Left (IdentifierNotFound _) -> Left $ IdentifierNotFound sid
            r -> r
      Just (gsid, _) -> Left $ NotNamespace gsid s'
lookupHierarchical cat sid@(UnqualifiedID s) =
    maybe (Left $ IdentifierNotFound sid) Right $ Map.lookup s cat

-- like lookupHierarchical, but don't include the ID.
(|@|) :: Catalogue -> ScopedID -> KErr Definition
c |@| i =  (either Left $ Right . snd) (lookupHierarchical c i)
infixl 5 |@| -- binds more strongly than comparisons, but less than arithmetic

makeNamespace :: ScopedID -> Catalogue -> Catalogue
makeNamespace sid cat =
    recurse sid [] 
    where
      recurse (QualifiedID s s') qualifiers =
          Map.singleton s
             (s `qualifiedByStrings` qualifiers,
              Namespace $ recurse s' (s:qualifiers))
      recurse (UnqualifiedID s) qualifiers =
          Map.singleton s
             (s `qualifiedByStrings` qualifiers,
              Namespace cat)
             
                                      
