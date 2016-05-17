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
(Scope p cat) |@+| (n,d) = Scope p (cat |+| (UnqualifiedID n, d))
infixl 6 |@+|

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
