module KindLang.Lib.Operators where

import Control.Monad.Except
import qualified Data.Map as Map
    
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.Value
import KindLang.Lib.CoreTypes
import KindLang.Runtime.Eval  -- fixme shouldn't need this!
    
findBinaryOperator :: String -> TypeDescriptor -> TypeDescriptor -> KErr AExpr
findBinaryOperator "+" t1 t2 =
    return $ AInternalRef
               (ExprAnnotation (FunctionType [t1, t2] t2) [])
               (coreId "(+)")
findBinaryOperator _ _ _ =
    throwError $ InternalError "haven't finished implementing operators"
-- ditto
findPrefixOperator :: String -> TypeDescriptor -> KErr AExpr
findPrefixOperator "-" t =
    return $ AInternalRef (ExprAnnotation (FunctionType [t] t) []) (coreId "(u-)")
findPrefixOperator _ _ =
    throwError $ InternalError "haven't finished implementing operators"

-- fixme this should be in its own file, and grab in stuff from elsewhere to!
standardInternalFunctions :: InternalFunctions
standardInternalFunctions =
    Map.fromList [
            ("addInt", \ ((KindInt a):(KindInt b):[]) -> KindInt (a+b))
    ]
