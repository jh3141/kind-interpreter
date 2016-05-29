{-# LANGUAGE ScopedTypeVariables #-}
-- | Note - this module should only be used by the definition of Scope, for which
-- it is an internal implementation detail.
module KindLang.Data.Catalogue where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Arrow
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.AST
import KindLang.Data.KStat
import qualified KindLang.Locale.ErrorMessages as ErrorMessages

-- | The type of catalogues.  Catalogues are a map from a hierarchical
-- "resolvable id" to tuples containing a "canonical id" and a "catalogue entry".
type Catalogue s = IdentMap (CatEntry s)

data CatEntry s = CatEntry Definition |
                  CatNamespace (Catalogue s)
                  deriving (Show, Eq)

-- | An empty catalogue.
newCatalogue :: Catalogue s
newCatalogue = Map.empty

makeCatEntry :: Definition -> CatEntry s
makeCatEntry = CatEntry

-- | @catAdd c rid cid def@ adds a new item for definition @def@ with the
-- resolvable id @rid@ and canonical id @cid@ to a catalogue @c@, creating
-- new namespaces as necessary, and returning the updated catalogue.
--
-- May throw a NotNamespace error if an attempt is made to insert an item into
-- a namespace but the namespace already exists as a non-namespace definition.
catAdd :: forall s .
          Catalogue s -> NSID -> NSID -> Definition -> KStat s (Catalogue s)
catAdd cat rid cid def =
    updatedCat cat rid []
    where
      updatedCat :: Catalogue s -> NSID -> [String] -> KStat s (Catalogue s)
      -- we've found the correct namespace to insert in
      updatedCat c (UnqualifiedID s) _ =
          return $ Map.insert s (cid,makeCatEntry def) c
      -- need to create a new namespace
      updatedCat c (QualifiedID s s') qualifiers | s `Map.notMember` c = do
          newNsCat <- updatedCat newCatalogue s' (s:qualifiers)
          return $ Map.insert s (s `qualifiedByStrings` qualifiers,
                                 CatNamespace newNsCat) c
      -- insert into existing namespace
      updatedCat c (QualifiedID s s') qualifiers =
          case Map.lookup s c of
            Just (nssid, CatNamespace nscat) -> do
                inserted <- updatedCat nscat s' (s:qualifiers)
                return $ Map.insert s (nssid, CatNamespace inserted) c
            Just (cid, _) -> throwError $ NotNamespace cid s'
            Nothing       -> throwError $ InternalError $
                             "Nothing unexpected while updating category to " ++
                             "add to existing namespace"

-- | An operator for invoking 'catAdd' with resolvable id equal to canonical id.
-- @cat |+~| (sid,def)@ adds identifier @sid@ with defintion @def@ to @cat@.
-- Binds more tightly than |@~|.
(|+~|) :: Catalogue s -> (NSID, Definition) -> KStat s (Catalogue s)
c |+~| (sid,def) = catAdd c sid sid def
infixl 6 |+~|

-- | An operator for invoking 'catAdd' with different resolvable and canonical
-- ids. @cat |++~| (rid,cid,def)@ adds resolvable identifier @rid@ for
-- canonical id @cid@ and definition @def@ to catalogue @cat@. Binds at same
-- level as |+~|.
(|++~|) :: Catalogue s -> (NSID, NSID, Definition) -> KStat s (Catalogue s)
c |++~| (rid, cid, def) = catAdd c rid cid def
infixl 6 |++~|

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
catalogueWithOnly :: Catalogue s -> [String] -> KStat s (Catalogue s)
catalogueWithOnly cat identifiers =
    return $ Map.filterWithKey (\k _ -> elem k identifiers) cat

-- | Look up an identifier in a catalogue, returning a tuple of the the
-- canonical identifier and definition for the item found, or an error
-- otherwise.
lookupHierarchical :: Catalogue s -> NSID -> KStat s IdentDefinition
lookupHierarchical cat sid@(QualifiedID s s') =
    case Map.lookup s cat of
      Nothing -> throwError $ IdentifierNotFound sid
      Just (_, CatNamespace cat2) ->
          catchError (lookupHierarchical cat2 s')
                     (throwError . replaceErrorIdentifier sid)
      Just (gsid, _) -> throwError $ NotNamespace gsid s'
lookupHierarchical cat sid@(UnqualifiedID s) =
    case Map.lookup s cat of
      Nothing -> throwError $ IdentifierNotFound sid -- fixme should be full id, not ns-relative
      Just (cid, CatEntry def) -> return (cid, def)
      Just (cid, CatNamespace _) -> throwError $ IsNamespace cid

-- | Like lookupHierarchical, but don't include the canonical ID in the result,
-- just the definition. Binds at level (infixl 5), i.e. stronger than
-- comparisons, but looser than arithmetic.
(|@~|) :: Catalogue s -> NSID -> KStat s Definition
c |@~| i =  (lookupHierarchical c i) >>= (return . snd)
infixl 5 |@~|

-- | 'makeNamespace sid cat' adds a new namespace with name 'sid' to catalogue
-- 'cat', returning the modified catalogue.
makeNamespace :: NSID -> Catalogue s -> KStat s (Catalogue s)
makeNamespace sid cat =
    return $ recurse sid []
    where
      recurse (QualifiedID s s') qualifiers =
          Map.singleton s
             (s `qualifiedByStrings` qualifiers,
              CatNamespace $ recurse s' (s:qualifiers))
      recurse (UnqualifiedID s) qualifiers =
          Map.singleton s
             (s `qualifiedByStrings` qualifiers,
              CatNamespace cat)


-- | Map a catalogue to a list of tuples containing the identifier by
-- which the item may be referenced, the canonical identifier of the item,
-- and its definition.
catFlatten :: Catalogue s -> KStat s [(NSID,NSID,Definition)]
catFlatten =
    return . Map.foldWithKey (processItem []) []
    where
      processItem :: [String] -> String -> (NSID, CatEntry s) ->
                     [(NSID,NSID,Definition)] ->
                     [(NSID,NSID,Definition)]
      processItem q k (i,CatNamespace c) t = Map.foldWithKey (processItem (k:q)) t c
      processItem q k (i,CatEntry d) t = (k `qualifiedByStrings` reverse q, i, d) : t

-- | Create a catalogue from a definition list, given a function that converts
-- plain strings to qualified ids.
catalogueForDefinitionList :: (String -> NSID) -> DefList -> KStat s (Catalogue s)
catalogueForDefinitionList makeNsid definitions =
    return $ Map.fromList
               ((identifyEntry >>>          -- extract the id to an outer tuple
                 second (first makeNsid >>> -- qualify the id in the inner tuple
                         second CatEntry))  -- and turn the definition into an entry
                <$> definitions)            -- over all definitions
    where
      identifyEntry (rid, def) = (rid, (rid, def))
