module KindLang.Runtime.Data where

import Data.STRef
import KindLang.Data.BasicTypes
import KindLang.Data.AnnotatedAST
import KindLang.Data.Catalogue
import KindLang.Data.Error
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
      scopeCat :: Catalogue stt (Value stt)
    }
    deriving (Show)

-- fixme probably want a lower-level implementation of this, so we can manage
-- memory ourselves
data Value s =
    KindUnit |
    KindInt Int |
    KindString String | -- nb temporary to allow some of our tests to work
    KindFunctionRef [AFunctionInstance] |
    KindRef (STRef s (Value s))
    deriving (Eq)

type DefinitionOrValue s = DefinitionOr (Value s)

type ValueOrRef s = Either (Value s) (STRef s (Value s))
    
