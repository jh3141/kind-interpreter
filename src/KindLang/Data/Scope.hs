{-# LANGUAGE ScopedTypeVariables #-}
module KindLang.Data.Scope where

import Control.Monad.Except
import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.KStat
import KindLang.Data.Types
    
-- | Scope associates names with definitions.  It is a nested structure (a scope
-- may have a parent scope, and if it does the definitions in that scope are
-- considered included in the child scope, except where a new definition with
-- the same name is present).
--
-- Scopes are intended to use inside an ST monad execution thread, so receive an
-- 'stt' type to identify the thread (i.e. if using "KStat s n", the scope's
-- parameter should be "s").
data Scope stt =
    Scope
    {
      scopeParent :: Maybe (Scope stt),
      scopeCat :: Catalogue stt
    }
    deriving (Show, Eq)

-- | Look up an identifier in a scope, returning its canonical id and
-- definition, or an error otherwise.
scopeLookup :: Scope s -> NSID -> KStat s IdentDefinition
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
makeFunctionScope s (FunctionType types _) names =
    foldM addVariableToScope (Scope (Just s) newCatalogue) (zip names types)
    where
      addVariableToScope :: Scope s -> (String,TypeDescriptor) -> KStat s (Scope s)
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
makeResolvedType :: NSID -> IdentDefinition -> TypeDescriptor
makeResolvedType sid (cid, def) = ResolvedType sid cid def

-- FIXME this has been removed from Types.hs in order to remove the dependency
-- from there to this module, but some of its logic clearly belongs there.
-- consider splitting it so the relevant parts can exist there.
-- | Look up a type and provide a canonical id for it
typeLookup :: Scope s -> TypeDescriptor -> KStat s TypeDescriptor
typeLookup s (SimpleType sid) = do
    (cid, def) <- scopeLookup s sid
    return (ResolvedType sid cid def)
typeLookup _ rt@(ResolvedType _ _ _) = return rt
typeLookup s (FunctionType parameters rtype) = do
    rparameters <- mapM (typeLookup s) parameters
    rrtype <- typeLookup s rtype
    return $ FunctionType rparameters rrtype
typeLookup _ InferableType = return InferableType
-- FIXME other type descriptor constructors must be included here!
