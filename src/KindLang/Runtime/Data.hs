module KindLang.Runtime.Data where

import Data.STRef
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Catalogue
import KindLang.Data.Error
import KindLang.Data.KStat
import KindLang.Data.Types

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
      scopeCat :: Catalogue stt Value
    }
    deriving (Show)

-- | Type of functions that can be used to provide a fully initialized variable
-- or constant instance from a definition.  Note that such a function may
-- necessarily execute user code, and therefore cannot be defined at the
-- low levels where it is required for initialization-on-demand.
type ItemInitializer s = Scope s -> Definition -> KStat s (TypeDescriptor, Value)

-- fixme probably want a lower-level implementation of this, so we can manage
-- memory ourselves
data Value =
    KindUnit |
    KindInt Int |
    KindString String | -- nb temporary to allow some of our tests to work
    KindFunctionRef [FunctionInstance]
    deriving (Show, Eq)

type DefinitionOrValue = DefinitionOr Value
