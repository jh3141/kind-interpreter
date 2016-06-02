module KindLang.Data.Value where

import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.KStat

-- fixme probably want a lower-level implementation of this, so we can manage memory
-- ourselves
data Value =
    KindUnit |
    KindInt Int |
    KindFunctionRef [FunctionInstance]
    deriving (Show, Eq)

-- | Type of functions that can be used to provide a fully initialized variable
-- or constant instance from a definition.  Note that such a function may
-- necessarily execute user code, and therefore cannot be defined at the
-- low levels where it is required for initialization-on-demand.
type ItemInitializer s = Definition -> KStat s (TypeDescriptor, Value)

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
                        
