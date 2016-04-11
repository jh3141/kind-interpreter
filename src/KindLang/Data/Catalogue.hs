module KindLang.Data.Catalogue where

import qualified Data.Map as Map
    
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.AST
    
type ModuleLoader = ScopedID -> KErr Catalogue
type Catalogue = IdentMap Definition

-- | An empty catalogue.
newCatalogue :: Catalogue
newCatalogue = Map.empty

-- | Add a new item to a catalogue, creating new namespaces as necessary.
-- May fail with an 'error' if an attempt is made to insert an item into
-- a namespace but the namespace already exists as a non-namespace definition,
-- therefore callers should be careful to avoid this situation or to catch
-- it in the IO monad.
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

-- | An operator for invoking 'catAdd'.  Second argument is a tuple
-- of the second and third arguments to catAdd (i.e. these arguments are
-- uncurried). Binds more tightly than |@|.
(|+|) :: Catalogue -> (ScopedID, Definition) -> Catalogue
(|+|) c = uncurry $ catAdd c
infixl 6 |+|
    
-- | Filter a catalogue to contain only the items specified in a list
-- of string identifiers.  Only examines top-level identifiers in the catalogue,
-- so not useful for operating across scope levels.
--
-- This implementation has a complexity that grows linearly
-- with the number of ids to retain.  It would be possible to
-- implement it so that the complexity grows more slowly than this,
-- but it is not entirely clear that this is useful.
-- also consider what existing typeclasses could be used instead of
-- a list of strings, in order to allow for the caller to decide what
-- is the most appropriate structure for them.
catalogueWithOnly :: Catalogue -> [String] -> Catalogue
catalogueWithOnly cat identifiers =
    Map.filterWithKey (\k _ -> elem k identifiers) cat

-- | Look up an identifier in a catalogue, returning a tuple of the the
-- canonical identifier and definition for the item found, or an error
-- otherwise.
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

-- | Like lookupHierarchical, but don't include the canonical ID in the result,
-- just the definition. Binds at level (infixl 5), i.e. stronger than
-- comparisons, but looser than arithmetic.
(|@|) :: Catalogue -> ScopedID -> KErr Definition
c |@| i =  (either Left $ Right . snd) (lookupHierarchical c i)
infixl 5 |@|

-- | 'makeNamespace sid cat' adds a new namespace with name 'sid' to catalogue
-- 'cat', returning the modified catalogue.
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
             

-- | Map a catalogue to a list of tuples containing the identifier by
-- which the item may be referenced, the canonical identifier of the item,
-- and its definition.
catFlatten :: Catalogue -> [(ScopedID,ScopedID,Definition)]
catFlatten =
    Map.foldWithKey (processItem []) []
    where
      processItem :: [String] -> String -> IdentDefinition ->
                     [(ScopedID,ScopedID,Definition)] ->
                     [(ScopedID,ScopedID,Definition)]
      processItem q k (i,Namespace c) t = Map.foldWithKey (processItem (k:q)) t c
      processItem q k (i,d) t = (k `qualifiedByStrings` reverse q, i, d) : t
                              
