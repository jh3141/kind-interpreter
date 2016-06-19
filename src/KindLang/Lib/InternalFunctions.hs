module KindLang.Lib.InternalFunctions where

import qualified Data.Map as Map
import KindLang.Data.AST  -- fixme shouldn't need this here!
import KindLang.Data.Value

type InternalFunction m s = [ValueOrRef s] -> m s (ValueOrRef s)
type InternalFunctions m s = Map.Map InternalFunctionName (InternalFunction m s)
