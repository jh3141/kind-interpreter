{-# LANGUAGE ConstraintKinds, TypeOperators, DataKinds #-}
module KindLang.Runtime.Data where

import Data.STRef
import Data.Array.ST
import Data.MultiConstrainedDynamic
import Data.Type.HasClass
import Data.Type.HasClassPreludeInstances
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Catalogue
import KindLang.Data.Error
import KindLang.Data.HasClassInstances
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

-- these are the type classes we want to maintain knowledge of when
-- we store an object dynamically. note that all values must have appropriate
-- instances of HasClass.
type BoxClasses = [ Show, Ord, Eq, Read ]

-- fixme probably want a lower-level implementation of this, so we can manage
-- memory ourselves
data Value s =
    KindUnit |
    KindInt Int |
    KindString String | -- nb temporary to allow some of our tests to work
    KindFunctionRef [FunctionInstance] |
    KindRef (STRef s (Value s)) |
    KindObject {
      kobjMetaclass :: STRef s (Value s),
      kobjSlots     :: STArray s Int (Value s)
    } |
    KindBox (MCDynamic BoxClasses)
    deriving (Eq)

type DefinitionOrValue s = DefinitionOr (Value s)

type ValueOrRef s = Either (Value s) (STRef s (Value s))

