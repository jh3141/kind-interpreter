module KindLang.Data.Catalogue where

import qualified Data.Map as Map
import Control.Monad.Except
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.AST
import KindLang.Data.KStat
import qualified KindLang.Locale.ErrorMessages as ErrorMessages

-- | A type for functions that are able to load the public catalogue from a
-- module with a specified id.
type ModuleLoader s = NSID -> KStat s Catalogue

-- | The type of catalogues.  Catalogues are a map from a hierarchical
-- "resolvable id" to tuples containing a "canonical id" and a "definition".
type Catalogue = IdentMap Definition

-- | An empty catalogue.
newCatalogue :: Catalogue
newCatalogue = Map.empty

-- | @catAdd c rid cid def@ adds a new item for definition @def@ with the
-- resolvable id @rid@ and canonical id @cid@ to a catalogue @c@, creating
-- new namespaces as necessary, and returning the updated catalogue.
--
-- May fail with an 'error' if an attempt is made to insert an item into
-- a namespace but the namespace already exists as a non-namespace definition,
-- therefore callers should be careful to avoid this situation or to catch
-- it in the IO monad.
catAdd :: Catalogue -> NSID -> NSID -> Definition -> Catalogue
catAdd cat rid cid def =
    updatedCat cat rid []
    where
      updatedCat c (UnqualifiedID s) _ =
          Map.insert s (cid,def) c
      updatedCat c (QualifiedID s s') qualifiers | s `Map.notMember` c =
          Map.insert s (s `qualifiedByStrings` qualifiers,
                        Namespace $ updatedCat newCatalogue s' (s:qualifiers)) c
      updatedCat c (QualifiedID s s') qualifiers =
          case Map.lookup s c of
            Just (nssid, Namespace nscat) ->
                Map.insert s
                       (nssid,
                        Namespace $ updatedCat nscat s' (s:qualifiers))
                       c
            _ -> error (ErrorMessages.insertedIntoNonNamespace rid)

-- | An operator for invoking 'catAdd' with resolvable id equal to canonical id.
-- @cat |+| (sid,def)@ adds identifier @sid@ with defintion @def@ to @cat@.
-- Binds more tightly than |@|.
(|+|) :: Catalogue -> (NSID, Definition) -> Catalogue
c |+| (sid,def) = catAdd c sid sid def
infixl 6 |+|

-- | An operator for invoking 'catAdd' with different resolvable and canonical
-- ids. @cat |++| (rid,cid,def)@ adds resolvable identifier @rid@ for
-- canonical id @cid@ and definition @def@ to catalogue @cat@. Binds at same
-- level as |+|.
(|++|) :: Catalogue -> (NSID, NSID, Definition) -> Catalogue
c |++| (rid, cid, def) = catAdd c rid cid def
infixl 6 |++|

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
lookupHierarchical :: Catalogue -> NSID -> KStat s IdentDefinition
lookupHierarchical cat sid@(QualifiedID s s') =
    case Map.lookup s cat of
      Nothing -> throwError $ IdentifierNotFound sid
      Just (_, Namespace cat2) ->
          catchError (lookupHierarchical cat2 s')
                     (throwError . replaceErrorIdentifier sid)
      Just (gsid, _) -> throwError $ NotNamespace gsid s'
lookupHierarchical cat sid@(UnqualifiedID s) =
    maybe (throwError $ IdentifierNotFound sid) return $ Map.lookup s cat

-- | Like lookupHierarchical, but don't include the canonical ID in the result,
-- just the definition. Binds at level (infixl 5), i.e. stronger than
-- comparisons, but looser than arithmetic.
(|@|) :: Catalogue -> NSID -> KStat s Definition
c |@| i =  (lookupHierarchical c i) >>= (return . snd)
infixl 5 |@|

-- | 'makeNamespace sid cat' adds a new namespace with name 'sid' to catalogue
-- 'cat', returning the modified catalogue.
makeNamespace :: NSID -> Catalogue -> Catalogue
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
catFlatten :: Catalogue -> [(NSID,NSID,Definition)]
catFlatten =
    Map.foldWithKey (processItem []) []
    where
      processItem :: [String] -> String -> IdentDefinition ->
                     [(NSID,NSID,Definition)] ->
                     [(NSID,NSID,Definition)]
      processItem q k (i,Namespace c) t = Map.foldWithKey (processItem (k:q)) t c
      processItem q k (i,d) t = (k `qualifiedByStrings` reverse q, i, d) : t

-- | Looks up a type in a catalogue by id and returns a type descriptor for it
-- where possible.
resolveType :: Catalogue -> NSID -> KStat s TypeDescriptor
resolveType cat sid =
    makeResolvedType sid <$> lookupHierarchical cat sid

-- | Creates a resolved type descriptor refering to an identified type
-- definition (e.g. a definition returned by 'lookupHierarchical').
makeResolvedType :: NSID -> IdentDefinition -> TypeDescriptor
makeResolvedType sid (cid, def) = ResolvedType sid cid def
