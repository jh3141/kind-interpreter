{-# LANGUAGE ScopedTypeVariables #-}
-- | Note - this module should only be used by the definition of Scope, for which
-- it is an internal implementation detail.
module KindLang.Data.Catalogue where

import qualified Data.HashTable.ST.Basic as HT
import qualified Data.HashTable.Class as HTC
import Control.Monad.Except
import Control.Monad.ST
import Control.Arrow
import Data.STRef
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.AST
import KindLang.Data.MStat
import qualified KindLang.Locale.ErrorMessages as ErrorMessages

-- | The type of catalogues.  Catalogues are a map from a hierarchical
-- "resolvable id" to tuples containing a "canonical id" and a "catalogue entry".
type Catalogue s v = HT.HashTable s String (NSID, CatEntry s v)

-- | An encapsulation of what information can be stored about an identifier
-- in a catalogue, i.e. either a definition or its type and value.
type DefinitionOr v = Either Definition (TypeDescriptor,v)

data CatEntry s v = CatEntry Definition |
                    CatEntryR TypeDescriptor (STRef s v) |
                    CatNamespace (Catalogue s v)

-- | An empty catalogue.
newCatalogue :: MStat m s => m s (Catalogue s v)
newCatalogue = liftToST HT.new

-- | A catalogue containing a single item
newCatalogueWith :: MStat m s =>
                    String -> NSID -> CatEntry s v -> m s (Catalogue s v)
newCatalogueWith k cid ent = do
    cat <- newCatalogue
    liftToST $ HT.insert cat k (cid,ent)
    return cat

makeCatEntry :: Definition -> CatEntry s v
makeCatEntry = CatEntry

makeCatEntryR :: MStat m s => TypeDescriptor -> v -> m s (CatEntry s v)
makeCatEntryR td v = CatEntryR td <$> (liftToST $ newSTRef v)

-- | @catAdd c rid cid def@ adds a new item for definition @def@ with the
-- resolvable id @rid@ and canonical id @cid@ to a catalogue @c@, creating
-- new namespaces as necessary, and returning the updated catalogue.
--
-- May throw a NotNamespace error if an attempt is made to insert an item into
-- a namespace but the namespace already exists as a non-namespace definition.
catAdd :: MStat m s => Catalogue s v -> NSID -> NSID -> Definition -> m s ()
catAdd cat rid cid def = catAddEntry cat rid (cid,makeCatEntry def)

catAddEntry :: forall m s v . MStat m s =>
               Catalogue s v -> NSID -> (NSID, CatEntry s v) -> m s ()
catAddEntry cat rid ent =
    updateCat cat rid []
    where
      updateCat :: Catalogue s v -> NSID -> [String] -> m s ()
      -- we've found the correct namespace to insert in
      updateCat c (UnqualifiedID s) _ =
          liftToST $ HT.insert c s ent
      -- insert into existing or new namespace under current namespace
      updateCat c (QualifiedID s s') qualifiers = do
          lookupResult <- liftToST $ HT.lookup c s
          case lookupResult of
            Just (nssid, CatNamespace nscat) ->
                             updateCat nscat s' (s:qualifiers)
            Just (cid, _) -> throwError $ NotNamespace cid s'
            Nothing       -> do
                               -- need to create a new namespace
                               newNsCat <- newCatalogue
                               updateCat newNsCat s' (s:qualifiers)
                               liftToST $
                                 HT.insert c s (s `qualifiedByStrings` qualifiers,
                                                CatNamespace newNsCat)

-- | Update an existing entry, or fail with an IdentifierNotFound error.
catUpdateEntry :: MStat m s => Catalogue s v -> NSID -> CatEntry s v -> m s ()
catUpdateEntry cat rid ent = do
    found <- findEntry cat rid
    case found of
      Nothing       -> throwError $ IdentifierNotFound rid
      Just (cid, _) -> catAddEntry cat rid (cid,ent)
-- FIXME above function scans the catalogue twice.  We can avoid this.

-- | An operator for invoking 'catAdd' with resolvable id equal to canonical id.
-- @cat |+~| (sid,def)@ adds identifier @sid@ with defintion @def@ to @cat@.
-- Binds more tightly than |@~|.  Returns @cat@ for convience of chaining.
(|+~|) :: MStat m s =>
          Catalogue s v -> (NSID, Definition) -> m s (Catalogue s v)
c |+~| (sid,def) = catAdd c sid sid def >> return c
infixl 6 |+~|

-- | An operator for invoking 'catAdd' with different resolvable and canonical
-- ids. @cat |++~| (rid,cid,def)@ adds resolvable identifier @rid@ for
-- canonical id @cid@ and definition @def@ to catalogue @cat@. Binds at same
-- level as |+~|.  Returns @cat@ for convienence of chaining.
(|++~|) :: MStat m s =>
           Catalogue s v -> (NSID, NSID, Definition) -> m s (Catalogue s v)
c |++~| (rid, cid, def) = catAdd c rid cid def >> return c
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
--
-- Creates a copy of the catalogue, rather than working on an existing
-- one.
catalogueWithOnly :: MStat m s => Catalogue s v -> [String] -> m s (Catalogue s v)
catalogueWithOnly cat identifiers = do
    newCat <- newCatalogue
    liftToST $ HTC.mapM_ (copyIfMatching newCat) cat
    return newCat
  where
    copyIfMatching newCat (k, v)
          | elem k identifiers = HT.insert newCat k v
          | otherwise          = return ()

-- | Look up an identifier in a catalogue, returning a tuple of the the
-- canonical identifier and definition for the item found, or an error
-- otherwise.
lookupHierarchical :: MStat m s =>
                      Catalogue s v -> NSID -> m s (NSID, DefinitionOr v)
lookupHierarchical cat sid = do
    ent <- findEntry cat sid
    case ent of
      Nothing                     -> throwError $ IdentifierNotFound sid 
      Just (cid, CatEntry def)    -> return (cid, Left def)
      Just (cid, CatEntryR td vr) -> do
                                        v <- liftToST $ readSTRef vr
                                        return (cid, Right (td, v))
      Just (cid, CatNamespace _)  -> throwError $ IsNamespace cid

-- | Look up an identifier in a catalogue, returning its canonical
-- id and entry details, or Nothing.  May throw an error if an identifier
-- is expected to resolve to a namespace but instead resolves to a different
-- object type.
findEntry :: MStat m s =>
             Catalogue s v -> NSID -> m s (Maybe (NSID, CatEntry s v))
findEntry cat sid@(QualifiedID s s') =  do
    lookupResult <- liftToST $ HT.lookup cat s
    case lookupResult of
      Nothing -> return Nothing
      Just (_, CatNamespace cat2) ->
          catchError (findEntry cat2 s')
                     (throwError . replaceErrorIdentifier sid)
      Just (gsid, _) -> throwError $ NotNamespace gsid s'
findEntry cat sid@(UnqualifiedID s) = liftToST $ HT.lookup cat s

-- | Like lookupHierarchical, but don't include the canonical ID in the result,
-- just the definition. Binds at level (infixl 5), i.e. stronger than
-- comparisons, but looser than arithmetic.
(|@~|) :: MStat m s =>
          Catalogue s v -> NSID -> m s (DefinitionOr v)
c |@~| i =  (lookupHierarchical c i) >>= (return . snd)
infixl 5 |@~|

-- | 'makeNamespace sid cat' creates a new catalogue containing an existing
-- catalogue as a namespace.
makeNamespace :: MStat m s => NSID -> Catalogue s v -> m s (Catalogue s v)
makeNamespace sid cat = do
    recurse sid []
    where
      recurse (UnqualifiedID s) qualifiers =
          newCatalogueWith s sid (CatNamespace cat)
      recurse (QualifiedID s s') qualifiers =
          (CatNamespace <$> recurse s' (s:qualifiers)) >>=
          newCatalogueWith s (s `qualifiedByStrings` qualifiers)

-- | Map a catalogue to a list of tuples containing the identifier by
-- which the item may be referenced, the canonical identifier of the item,
-- and its definition.  Useful for debugging/testing.
catFlatten :: forall m s v . MStat m s =>
              Catalogue s v -> m s [(NSID,NSID,DefinitionOr v)]
catFlatten cat =
    liftToST $ HT.foldM (processItem []) [] cat
    where
      processItem :: [String] -> [(NSID,NSID,DefinitionOr v)] ->
                     (String, (NSID, CatEntry s v)) ->
                     ST s [(NSID,NSID,DefinitionOr v)]
      processItem q accum (k, (i, CatNamespace cat')) =
          HT.foldM (processItem (k:q)) accum cat'
      processItem q accum (k, (i, CatEntry d)) =
          return $ (k `qualifiedByStrings` reverse q, i, Left d) : accum
      processItem q accum (k, (i, CatEntryR td vr)) = do
          v <- readSTRef vr
          return $ (k `qualifiedByStrings` reverse q, i, Right (td, v)) : accum

-- | Extract the canonical ID from a flattened catalogue entry
flatCid :: (NSID, NSID, a) -> NSID
flatCid (_,cid,_) = cid

-- | Extract the relative ID from a flattened catalogue entry
flatRid :: (NSID, NSID, a) -> NSID
flatRid (rid,_,_) = rid

-- | Create a catalogue from a definition list, given a function that converts
-- plain strings to qualified ids.
catalogueForDefinitionList :: MStat m s => (String -> NSID) -> DefList ->
                              m s (Catalogue s v)
catalogueForDefinitionList makeNsid definitions =
    liftToST $ HTC.fromList
           ((identifyEntry >>>          -- extract the id to an outer tuple
             second (first makeNsid >>> -- qualify the id in the inner tuple
                     second CatEntry))  -- and turn the definition into an entry
            <$> definitions)            -- over all definitions
    where
      identifyEntry (rid, def) = (rid, (rid, def))

catalogueCopyTo :: MStat m s => Catalogue s v -> Catalogue s v -> m s ()
catalogueCopyTo dst src = liftToST $ HTC.mapM_ copyToDst src
    where
      copyToDst (k,v) = HT.insert dst k v
