{-# LANGUAGE ScopedTypeVariables #-}
module KindLang.Data.Scope where

import Control.Monad.Except
import KindLang.Data.BasicTypes
import KindLang.Data.Catalogue
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.KStat

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

-- | Adds an item with an unqualified identifier and a definition to a scope.
(|@+|) :: Scope s -> (String,Definition) -> Scope s
(Scope p cat) |@+| (n,d) = Scope p (cat |+~| (UnqualifiedID n, d))
infixl 6 |@+|

-- | Add an item to the scope using its canonical identifier as its resolvable
-- id (a shortcut to the more flexible '(|++|)').
(|+|) :: Scope s -> (NSID, Definition) -> Scope s
(Scope p cat) |+| identDef = Scope p (cat |+~| identDef)
infixl 6 |+|

-- | Add an item to the scope with both its resolvable and canonical ids
-- specified.
(|++|) :: Scope s -> (NSID, NSID, Definition) -> Scope s
(Scope p cat) |++| identCanidDef = Scope p (cat |++~| identCanidDef)
infixl 6 |++|

-- | 'makeFunctionScope s td names' makes a new Scope with parent 's'
-- containing the list of arguments for a function whose type is 'td' and whose
-- argument names are given as 'names'.  If 'td' does not identify a function
-- type, returns 's'.
makeFunctionScope :: Scope s -> TypeDescriptor -> [String] -> Scope s
makeFunctionScope s (FunctionType types _) names =
    foldr addVariableToScope (Scope (Just s) newCatalogue) (zip names types)
    where
      addVariableToScope :: (String,TypeDescriptor) -> Scope s -> Scope s
      addVariableToScope (name,td) ss =
          ss |@+| (name, VariableDefinition td VarInitNone)
makeFunctionScope s _ _ = s

-- | Looks up a type in a catalogue by id and returns a type descriptor for it
-- where possible.
resolveType :: Scope s -> NSID -> KStat s TypeDescriptor
resolveType s sid =
    makeResolvedType sid <$> scopeLookup s sid

-- | Creates a resolved type descriptor refering to an identified type
-- definition (e.g. a definition returned by 'scopeLookup').
makeResolvedType :: NSID -> IdentDefinition -> TypeDescriptor
makeResolvedType sid (cid, def) = ResolvedType sid cid def
