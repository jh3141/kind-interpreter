module KindLang.Data.Value
        (module KindLang.Data.Value,
         Value(..), ValueOrRef) where                -- rexported

import Data.STRef
import Data.Dynamic
import Data.Typeable
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Runtime.Data

type ValueRef s = STRef s (Value s)

-- Note that the 'get' functions are incomplete.  This should be fine, as assuming
-- the program we are evaluating type-checks correctly, they should never be used
-- on a value that is the wrong type.
getKindInt :: Value s -> Int
getKindInt (KindInt a) = a

makeKindInt :: Int -> Value s
makeKindInt a = KindInt a

getKindFunctionRef :: Value s -> [FunctionInstance]
getKindFunctionRef (KindFunctionRef a) = a

makeKindFunctionRef :: [FunctionInstance] -> Value s
makeKindFunctionRef a = KindFunctionRef a

makeKindString :: String -> Value s
makeKindString val = KindString val

makeKindBox :: Typeable a => a -> Value s
makeKindBox v = KindBox $ toDyn v

extractKindBox :: Typeable a => Value s -> Maybe a
extractKindBox (KindBox d) = fromDynamic d
extractKindBox _ = Nothing

instance Show (Value s) where
    show KindUnit = "(unit)"
    show (KindInt v) = show v
    show (KindString s) = show s
    show (KindFunctionRef insts) = "(fn " ++ show insts ++ ")"
    show (KindRef _) = "(reference)"
    show (KindBox b) = "(box: " ++ show b ++ ")"
