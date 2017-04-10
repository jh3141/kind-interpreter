module KindLang.Lib.InternalFunctions where

import qualified Data.Map as Map
import KindLang.Data.BasicTypes
import KindLang.Data.Value

type InternalFunction m s = [ValueOrRef s] -> m s (ValueOrRef s)
type InternalFunctions m s = Map.Map InternalFunctionName (InternalFunction m s)
