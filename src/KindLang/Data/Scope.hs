{-# LANGUAGE ScopedTypeVariables #-}
module KindLang.Data.Scope
    (module KindLang.Data.Scope,
     DefinitionOrValue,          -- reexported from Catalogue
     Scope,                      -- reexported from KindLang.Runtime.Data
     ItemInitializer)            -- reexported from KindLang.Runtime.Data
    where

import Control.Arrow
import Control.Monad.Except
import Data.STRef
import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.MStat
import KindLang.Data.Types
import KindLang.Data.Value
import KindLang.Runtime.Data

-- | Type of functions that can be used to provide a fully initialized variable
-- or constant instance from a definition.  Note that such a function may
-- necessarily execute user code, and therefore cannot be defined at the
-- low levels where it is required for initialization-on-demand.
type ItemInitializer m s = Scope s -> Definition -> m s (TypeDescriptor, Value)

-- | Look up an identifier in a scope, returning its canonical id and
-- either definition or type and value, or an error otherwise.
scopeLookup :: MStat m s => Scope s -> NSID -> m s (NSID, DefinitionOrValue)
scopeLookup s i =
    catchError (lookupHierarchical (scopeCat s) i)
               (deferToParent s)
    where
      deferToParent (Scope Nothing _) err = throwError err
      deferToParent (Scope (Just p) _) _  = scopeLookup p i

-- | Begins a chain of operations on a scope that require the use of the KStat
-- monad.
scopeUpdate :: MStat m s => Scope s -> m s (Scope s)
scopeUpdate = return

-- | Adds an item with an unqualified identifier and a definition to a scope.
(|@+|) :: MStat m s => m s (Scope s) -> (String,Definition) -> m s (Scope s)
scope |@+| (n,d) = scope >>= \ (Scope p cat) ->
                   Scope p <$> cat |+~| (UnqualifiedID n, d)
infixl 6 |@+|

-- | Add an item to the scope using its canonical identifier as its resolvable
-- id (a shortcut to the more flexible '(|++|)').
(|+|) :: MStat m s => m s (Scope s) -> (NSID, Definition) -> m s (Scope s)
scope |+| identDef = scope >>= \ (Scope p cat) -> Scope p <$> cat |+~| identDef
infixl 6 |+|

-- | Add an item to the scope with both its resolvable and canonical ids
-- specified.
(|++|) :: MStat m s => m s (Scope s) -> (NSID, NSID, Definition) -> m s (Scope s)
scope |++| identCanidDef = scope >>= \ (Scope p cat) ->
                           Scope p <$> cat |++~| identCanidDef
infixl 6 |++|

-- | 'makeFunctionScope s td names' makes a new Scope with parent 's'
-- containing the list of arguments for a function whose type is 'td' and whose
-- argument names are given as 'names'.  If 'td' does not identify a function
-- type, throws an internal error.
makeFunctionScope :: forall m s . MStat m s =>
                     Scope s -> TypeDescriptor -> [String] -> m s (Scope s)
makeFunctionScope s (FunctionType types _) names = do
    newCat <- newCatalogue
    foldM addVariableToScope (Scope (Just s) newCat) (zip names types)
    where
      addVariableToScope :: Scope s -> (String,TypeDescriptor) ->
                            m s (Scope s)
      addVariableToScope ss (name,td) =
          scopeUpdate ss |@+| (name, VariableDefinition td VarInitNone)
makeFunctionScope _ td _ = throwError $ InternalError
                           ("Expected a function, got " ++ typeName td)

-- | Looks up a type in a catalogue by id and returns a type descriptor for it
-- where possible.
resolveType :: MStat m s => Scope s -> NSID -> m s TypeDescriptor
resolveType s sid =
    makeResolvedType sid <$> scopeLookup s sid

-- | Utility function for resolving a type from a scope held in the KS monad
resolveTypeKS :: MStat m s => m s (Scope s) -> NSID -> m s TypeDescriptor
resolveTypeKS kss sid = kss >>= (flip resolveType) sid

-- | Creates a resolved type descriptor refering to an identified type
-- definition (e.g. a definition returned by 'scopeLookup').
makeResolvedType :: NSID -> (NSID, DefinitionOrValue) -> TypeDescriptor
makeResolvedType sid (cid, Left def) = ResolvedType sid cid def
makeResolvedType _ (_, Right _) =
    error "makeResolvedType should handle values as well as definitions"
-- FIXME what do we do when values are returned?  we're not after the type
-- descriptor of the value (which may, for instance, be a metaclass), but the
-- type descriptor for the value itself.

-- FIXME this has been removed from Types.hs in order to remove the dependency
-- from there to this module, but some of its logic clearly belongs there.
-- consider splitting it so the relevant parts can exist there.
-- | Look up a type and provide a canonical id for it
typeLookup :: MStat m s => Scope s -> TypeDescriptor -> m s TypeDescriptor
typeLookup s (SimpleType sid) =
    makeResolvedType sid <$> scopeLookup s sid
typeLookup _ rt@(ResolvedType _ _ _) = return rt
typeLookup s (FunctionType parameters rtype) = do
    rparameters <- mapM (typeLookup s) parameters
    rrtype <- typeLookup s rtype
    return $ FunctionType rparameters rrtype
typeLookup _ InferableType = return InferableType
-- FIXME other type descriptor constructors must be included here!

scopeLookupRef :: MStat m s => Scope s -> NSID -> ItemInitializer m s ->
                  m s (NSID, STRef s Value)
scopeLookupRef sc i initializer = do
    found <- findEntry (scopeCat sc) i
    case found of
      Nothing                    -> passRequestUp (scopeParent sc)
      Just (cid, CatEntry def)   -> initializeRef initializer sc cid i def
      Just (cid, CatEntryR _ r)  -> return (cid, r)
      Just (cid, CatNamespace _) -> throwError $ IsNamespace cid
    where
      passRequestUp (Just scp) = scopeLookupRef scp i initializer
      passRequestUp Nothing    = throwError $ IdentifierNotFound i

scopeLookupValue :: MStat m s =>
                    Scope s -> NSID -> ItemInitializer m s -> m s (NSID, Value)
scopeLookupValue sc i ii = scopeLookupRef sc i ii >>=
                           (runKleisli $ second (Kleisli kstatReadRef))

-- | Initialize a definition to a runtime variable
initializeRef :: MStat m s =>
                 ItemInitializer m s -> Scope s -> NSID -> NSID -> Definition ->
                 m s (NSID, STRef s Value)
initializeRef initializer sc cid i def = do
    (td, val) <- initializer sc def
    ref <- kstatNewRef val
    catUpdateEntry (scopeCat sc) i
                   (CatEntryR td ref)
    return (cid, ref)

scopeAddItems :: MStat m s => Scope s -> [(NSID,TypeDescriptor,Value)] -> m s ()
scopeAddItems scope values = mapM_ (scopeAddItem scope) values

scopeAddItem :: MStat m s => Scope s -> (NSID,TypeDescriptor,Value) -> m s ()
scopeAddItem sc (sid,td,val) = do
    ref <- kstatNewRef val
    catAddEntry (scopeCat sc) sid (sid, CatEntryR td ref)

scopeItems :: MStat m s => Scope s -> m s [(NSID, NSID, DefinitionOrValue)]
scopeItems sc = catFlatten $ scopeCat sc
