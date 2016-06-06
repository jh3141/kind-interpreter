module KindLang.Lib.InternalFunctions where

import qualified Data.Map as Map
import KindLang.Data.AST  -- fixme shouldn't need this here!
import KindLang.Data.Value

type InternalFunctions = Map.Map InternalFunctionName ([Value] -> Value)
