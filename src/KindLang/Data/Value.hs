module KindLang.Data.Value
        (module KindLang.Data.Value,
         Value) where

import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.KStat
import KindLang.Runtime.Data

-- Note that the 'get' functions are incomplete.  This should be fine, as assuming
-- the program we are evaluating type-checks correctly, they should never be used
-- on a value that is the wrong type.
getKindInt :: Value -> Int
getKindInt (KindInt a) = a

makeKindInt :: Int -> Value
makeKindInt a = KindInt a

getKindFunctionRef :: Value -> [FunctionInstance]
getKindFunctionRef (KindFunctionRef a) = a

makeKindFunctionRef :: [FunctionInstance] -> Value
makeKindFunctionRef a = KindFunctionRef a

makeKindString :: String -> Value
makeKindString val = KindString val
