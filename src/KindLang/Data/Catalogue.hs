{-# LANGUAGE ScopedTypeVariables #-}
-- | Note - this module should only be used by the definition of Scope, for which
-- it is an internal implementation detail.
module KindLang.Data.Catalogue where

import qualified Data.HashTable.ST.Basic as HT
import qualified Data.HashTable.Class as HTC
import Control.Monad.Except
import Control.Monad.ST
import Control.Arrow
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.AST
import KindLang.Data.KStat
import qualified KindLang.Locale.ErrorMessages as ErrorMessages

-- | The type of catalogues.  Catalogues are a map from a hierarchical
-- "resolvable id" to tuples containing a "canonical id" and a "catalogue entry".
type Catalogue s = HT.HashTable s String (NSID, CatEntry s)

data CatEntry s = CatEntry Definition |
                  CatNamespace (Catalogue s)
                  deriving (Show)

-- | An empty catalogue.
newCatalogue :: KStat s (Catalogue s)
newCatalogue = liftToST HT.new

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
    updateCat cat rid [] >> return cat
    where
      updateCat :: Catalogue s -> NSID -> [String] -> KStat s ()
      -- we've found the correct namespace to insert in
      updateCat c (UnqualifiedID s) _ =
          liftToST $ HT.insert c s (cid,makeCatEntry def)
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
lookupHierarchical :: Catalogue s -> NSID -> KStat s IdentDefinition
lookupHierarchical cat sid@(QualifiedID s s') = do
    lookupResult <- liftToST $ HT.lookup cat s
    case lookupResult of
      Nothing -> throwError $ IdentifierNotFound sid
      Just (_, CatNamespace cat2) ->
          catchError (lookupHierarchical cat2 s')
                     (throwError . replaceErrorIdentifier sid)
      Just (gsid, _) -> throwError $ NotNamespace gsid s'
lookupHierarchical cat sid@(UnqualifiedID s) = do
    lookupResult <- liftToST $ HT.lookup cat s
    case lookupResult of
      Nothing                    -> throwError $ IdentifierNotFound sid 
      Just (cid, CatEntry def)   -> return (cid, def)
      Just (cid, CatNamespace _) -> throwError $ IsNamespace cid

-- | Like lookupHierarchical, but don't include the canonical ID in the result,
-- just the definition. Binds at level (infixl 5), i.e. stronger than
-- comparisons, but looser than arithmetic.
(|@~|) :: Catalogue s -> NSID -> KStat s Definition
c |@~| i =  (lookupHierarchical c i) >>= (return . snd)
infixl 5 |@~|

-- | 'makeNamespace sid cat' adds a new namespace with name 'sid' to catalogue
-- 'cat'.
makeNamespace :: NSID -> Catalogue s -> KStat s ()
makeNamespace sid cat =
    recurse sid cat []
    where
      recurse sid@(UnqualifiedID s) cat qualifiers =
          do
            lookupResult <- liftToST $ HT.lookup cat s
            emptyCatalogue <- newCatalogue
            case lookupResult of
              Nothing -> liftToST $ HT.insert cat s
                              (s `qualifiedByStrings` reverse qualifiers,
                               CatNamespace emptyCatalogue)
              Just (_, CatNamespace _)
                      -> return ()
              Just (cid, _)
                      -> throwError $ NotNamespace cid sid

      recurse sid@(QualifiedID s s') cat qualifiers =
          do
            lookupResult <- liftToST $ HT.lookup cat s
            case lookupResult of
              Nothing -> do
                sub <- newCatalogue
                recurse s' sub (s:qualifiers)
                liftToST $
                  HT.insert cat s (s `qualifiedByStrings` reverse qualifiers,
                                   CatNamespace sub)
              Just (_, CatNamespace sub)
                      -> recurse s' sub (s:qualifiers)
              Just (cid, _)
                      -> throwError $ NotNamespace cid sid

-- | Map a catalogue to a list of tuples containing the identifier by
-- which the item may be referenced, the canonical identifier of the item,
-- and its definition.
catFlatten :: forall s . Catalogue s -> KStat s [(NSID,NSID,Definition)]
catFlatten cat =
    liftToST $ HT.foldM (processItem []) [] cat
    where
      processItem :: [String] -> [(NSID,NSID,Definition)] ->
                     (String, (NSID, CatEntry s)) ->
                     ST s [(NSID,NSID,Definition)]
      processItem q accum (k, (i, CatNamespace cat')) =
          HT.foldM (processItem (k:q)) accum cat'
      processItem q accum (k, (i, CatEntry d)) =
          return $ (k `qualifiedByStrings` reverse q, i, d) : accum

-- | Extract the canonical ID from a flattened catalogue entry
flatCid :: (NSID, NSID, Definition) -> NSID
flatCid (_,cid,_) = cid

-- | Create a catalogue from a definition list, given a function that converts
-- plain strings to qualified ids.
catalogueForDefinitionList :: (String -> NSID) -> DefList -> KStat s (Catalogue s)
catalogueForDefinitionList makeNsid definitions =
    liftToST $ HTC.fromList
           ((identifyEntry >>>          -- extract the id to an outer tuple
             second (first makeNsid >>> -- qualify the id in the inner tuple
                     second CatEntry))  -- and turn the definition into an entry
            <$> definitions)            -- over all definitions
    where
      identifyEntry (rid, def) = (rid, (rid, def))

catalogueCopyTo :: Catalogue s -> Catalogue s -> KStat s ()
catalogueCopyTo src dst = liftToST $ HTC.mapM_ copyToDst src
    where
      copyToDst (k,v) = HT.insert dst k v
