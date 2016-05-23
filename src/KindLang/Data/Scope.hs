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
data Scope =
    Scope
    {
      scopeParent :: Maybe Scope,
      scopeCat :: Catalogue
    }
    deriving (Show, Eq)

-- | Look up an identifier in a scope, returning its canonical id and
-- definition, or an error otherwise.
scopeLookup :: Scope -> NSID -> KStat s IdentDefinition
scopeLookup s i =
    catchError (lookupHierarchical (scopeCat s) i)
               (deferToParent s)
    where
      deferToParent (Scope Nothing _) err = throwError err
      deferToParent (Scope (Just p) _) _  = scopeLookup p i

-- | Adds an item with an unqualified identifier and a definition to a scope.
(|@+|) :: Scope -> (String,Definition) -> Scope
(Scope p cat) |@+| (n,d) = Scope p (cat |+~| (UnqualifiedID n, d))
infixl 6 |@+|

-- | Add an item to the scope using its canonical identifier as its resolvable
-- id (a shortcut to the more flexible '(|++|)').
(|+|) :: Scope -> (NSID, Definition) -> Scope
(Scope p cat) |+| identDef = Scope p (cat |+~| identDef)
infixl 6 |+|

-- | Add an item to the scope with both its resolvable and canonical ids
-- specified.
(|++|) :: Scope -> (NSID, NSID, Definition) -> Scope
(Scope p cat) |++| identCanidDef = Scope p (cat |++~| identCanidDef)
infixl 6 |++|

-- | Like 'scopeLookup', but don't include the canonical ID in the result,
-- just the definition. Binds at level (infixl 5), i.e. stronger than
-- comparisons, but looser than arithmetic.
(|@|) :: Scope -> NSID -> KStat s Definition
s |@| i =  (scopeLookup s i) >>= (return . snd)
infixl 5 |@|

-- | 'makeFunctionScope s td names' makes a new Scope with parent 's'
-- containing the list of arguments for a function whose type is 'td' and whose
-- argument names are given as 'names'.  If 'td' does not identify a function
-- type, returns 's'.
makeFunctionScope :: Scope -> TypeDescriptor -> [String] -> Scope
makeFunctionScope s (FunctionType types _) names =
    foldr addVariableToScope (Scope (Just s) newCatalogue) (zip names types)
    where
      addVariableToScope :: (String,TypeDescriptor) -> Scope -> Scope
      addVariableToScope (name,td) ss =
          ss |@+| (name, VariableDefinition td VarInitNone)
makeFunctionScope s _ _ = s

-- | Looks up a type in a catalogue by id and returns a type descriptor for it
-- where possible.
resolveType :: Scope -> NSID -> KStat s TypeDescriptor
resolveType s sid =
    makeResolvedType sid <$> scopeLookup s sid

-- | Creates a resolved type descriptor refering to an identified type
-- definition (e.g. a definition returned by 'scopeLookup').
makeResolvedType :: NSID -> IdentDefinition -> TypeDescriptor
makeResolvedType sid (cid, def) = ResolvedType sid cid def
