module KindLang.Lib.Operators where

import Control.Monad.Except
import qualified Data.Map as Map
import Data.List

import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.Value
import KindLang.Data.KStat
import KindLang.Data.Scope
import KindLang.Lib.CoreTypes
import KindLang.Lib.InternalFunctions
import KindLang.Runtime.Data
    
findBinaryOperator :: String -> TypeDescriptor -> TypeDescriptor -> KStat s AExpr
findBinaryOperator "+" t1 t2 =
    return $ AVarRef
               (ExprAnnotation (FunctionType [t1, t2] t2) [])
               (coreId "(+)")
findBinaryOperator _ _ _ =
    throwError $ InternalError "haven't finished implementing operators"
-- ditto
findPrefixOperator :: String -> TypeDescriptor -> KStat s AExpr
findPrefixOperator "-" t =
    return $ AVarRef (ExprAnnotation (FunctionType [t] t) []) (coreId "(u-)")
findPrefixOperator _ _ =
    throwError $ InternalError "haven't finished implementing operators"


makeInternalFn :: [TypeDescriptor] -> TypeDescriptor -> InternalFunctionName ->
                  FunctionInstance
makeInternalFn a r n = InternalFunction (FunctionType a r) n

addStandardOperatorsToScope :: Scope s -> KStat s (Scope s)
addStandardOperatorsToScope sc =
    scopeUpdate sc
        |++| (coreId "(+)", coreId "(+)", FunctionDefinition [
                makeInternalFn [rtKindInt, rtKindInt] rtKindInt "(+)II"])

-- fixme this should be in its own file, and grab in stuff from elsewhere to!
standardInternalFunctions :: InternalFunctions
standardInternalFunctions =
    Map.fromList [
            ("(+)II",
              \ ((KindInt a):(KindInt b):[]) -> KindInt (a+b))
    ]

