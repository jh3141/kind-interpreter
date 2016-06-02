{-# LANGUAGE ScopedTypeVariables #-}
module KindLang.Data.Scope
    (module KindLang.Data.Scope,
     DefinitionOrValue)          -- reexported from Catalogue
    where

import Control.Monad.Except
import Data.STRef
import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.KStat
import KindLang.Data.Types
import KindLang.Data.Value
    
-- | Scope associates names with types and values.  It is a nested structure
-- (a scope may have a parent scope, and if it does the definitions in that
-- scope are considered included in the child scope, except where a new
-- definition with the same name is present).
--
-- Scopes are intended for use inside an ST monad execution thread, so receive
-- an 'stt' type to identify the thread (i.e. if using "KStat s n", the scope's
-- parameter should be "s").
data Scope stt =
    Scope
    {
      scopeParent :: Maybe (Scope stt),
      scopeCat :: Catalogue stt
    }
    deriving (Show)

-- | Look up an identifier in a scope, returning its canonical id and
-- either definition or type and value, or an error otherwise.
scopeLookup :: Scope s -> NSID -> KStat s (NSID, DefinitionOrValue)
scopeLookup s i =
    catchError (lookupHierarchical (scopeCat s) i)
               (deferToParent s)
    where
      deferToParent (Scope Nothing _) err = throwError err
      deferToParent (Scope (Just p) _) _  = scopeLookup p i

-- | Begins a chain of operations on a scope that require the use of the KStat
-- monad.
scopeUpdate :: Scope s -> KStat s (Scope s)
scopeUpdate = return

-- | Adds an item with an unqualified identifier and a definition to a scope.
(|@+|) :: KStat s (Scope s) -> (String,Definition) -> KStat s (Scope s)
scope |@+| (n,d) = scope >>= \ (Scope p cat) ->
                   Scope p <$> cat |+~| (UnqualifiedID n, d)
infixl 6 |@+|

-- | Add an item to the scope using its canonical identifier as its resolvable
-- id (a shortcut to the more flexible '(|++|)').
(|+|) :: KStat s (Scope s) -> (NSID, Definition) -> KStat s (Scope s)
scope |+| identDef = scope >>= \ (Scope p cat) -> Scope p <$> cat |+~| identDef
infixl 6 |+|

-- | Add an item to the scope with both its resolvable and canonical ids
-- specified.
(|++|) :: KStat s (Scope s) -> (NSID, NSID, Definition) -> KStat s (Scope s)
scope |++| identCanidDef = scope >>= \ (Scope p cat) ->
                           Scope p <$> cat |++~| identCanidDef
infixl 6 |++|

-- | 'makeFunctionScope s td names' makes a new Scope with parent 's'
-- containing the list of arguments for a function whose type is 'td' and whose
-- argument names are given as 'names'.  If 'td' does not identify a function
-- type, throws an internal error.
makeFunctionScope :: Scope s -> TypeDescriptor -> [String] -> KStat s (Scope s)
makeFunctionScope s (FunctionType types _) names = do
    newCat <- newCatalogue
    foldM addVariableToScope (Scope (Just s) newCat) (zip names types)
    where
      addVariableToScope :: Scope s -> (String,TypeDescriptor) ->
                            KStat s (Scope s)
      addVariableToScope ss (name,td) =
          scopeUpdate ss |@+| (name, VariableDefinition td VarInitNone)
makeFunctionScope _ td _ = throwError $ InternalError
                           ("Expected a function, got " ++ typeName td)

-- | Looks up a type in a catalogue by id and returns a type descriptor for it
-- where possible.
resolveType :: Scope s -> NSID -> KStat s TypeDescriptor
resolveType s sid =
    makeResolvedType sid <$> scopeLookup s sid

-- | Utility function for resolving a type from a scope held in the KS monad
resolveTypeKS :: KStat s (Scope s) -> NSID -> KStat s TypeDescriptor
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
typeLookup :: Scope s -> TypeDescriptor -> KStat s TypeDescriptor
typeLookup s (SimpleType sid) =
    makeResolvedType sid <$> scopeLookup s sid
typeLookup _ rt@(ResolvedType _ _ _) = return rt
typeLookup s (FunctionType parameters rtype) = do
    rparameters <- mapM (typeLookup s) parameters
    rrtype <- typeLookup s rtype
    return $ FunctionType rparameters rrtype
typeLookup _ InferableType = return InferableType
-- FIXME other type descriptor constructors must be included here!

scopeLookupRef :: Scope s -> NSID -> ItemInitializer s -> KStat s (STRef s Value)
scopeLookupRef sc i initializer = do
    found <- findEntry (scopeCat sc) i
    case found of
      Nothing                    -> passRequestUp (scopeParent sc)
      Just (cid, CatEntry def)   -> initializeRef initializer sc cid i def
      Just (cid, CatEntryR _ r)  -> return r
      Just (cid, CatNamespace _) -> throwError $ IsNamespace cid
    where
      passRequestUp (Just scp) = scopeLookupRef scp i initializer
      passRequestUp Nothing    = throwError $ IdentifierNotFound i

-- | Initialize a definition to a runtime variable
initializeRef :: ItemInitializer s -> Scope s -> NSID -> NSID -> Definition ->
                 KStat s (STRef s Value)
initializeRef initializer sc cid i def = do
    (td, val) <- initializer def
    ref <- kstatNewRef val
    catUpdateEntry (scopeCat sc) i
                   (CatEntryR td ref)
    return ref

scopeAddItems :: Scope s -> [(NSID,TypeDescriptor,Value)] -> KStat s ()
scopeAddItems scope values = mapM_ (scopeAddItem scope) values

scopeAddItem :: Scope s -> (NSID,TypeDescriptor,Value) -> KStat s ()
scopeAddItem sc (sid,td,val) = do
    ref <- kstatNewRef val
    catAddEntry (scopeCat sc) sid (sid, CatEntryR td ref)
