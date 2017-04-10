module KindLang.Lib.Operators where

import Control.Monad.Except
import qualified Data.Map as Map
import Data.List

import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.AnnotatedAST
import KindLang.Data.Error
import KindLang.Data.Value
import KindLang.Data.KStat
import KindLang.Data.MStat
import KindLang.Data.Scope
import KindLang.Lib.CoreTypes
import KindLang.Lib.InternalFunctions
import KindLang.Runtime.Data
    
findBinaryOperator :: String -> TypeDescriptor -> TypeDescriptor -> KStat s AExpr
findBinaryOperator "+" t1 t2 =
    return $ AVarRef
               (ExprAnnotation (FunctionType [t1, t2] t2) [])
               (coreId "(+)")
findBinaryOperator "=" (Reference t1) t2 =
    return $ AVarRef
               (ExprAnnotation (FunctionType [Reference t1, t2] t2) [])
               (coreId "(=)")
findBinaryOperator _ _ _ =
    throwError $ InternalError "haven't finished implementing operators"
-- ditto
findPrefixOperator :: String -> TypeDescriptor -> KStat s AExpr
findPrefixOperator "-" t =
    return $ AVarRef (ExprAnnotation (FunctionType [t] t) []) (coreId "(u-)")
findPrefixOperator _ _ =
    throwError $ InternalError "haven't finished implementing operators"


makeInternalFn :: [TypeDescriptor] -> TypeDescriptor -> InternalFunctionName ->
                  AFunctionInstance
makeInternalFn a r n = AInternalFunction (FunctionType a r) n

addStandardOperatorsToScope :: Scope s -> KStat s (Scope s)
addStandardOperatorsToScope sc =
    scopeUpdate sc
        |++| (coreId "(+)", coreId "(+)", AFunctionDefinition [
                makeInternalFn [rtKindInt, rtKindInt] rtKindInt "(+)II"])
        |++| (coreId "(=)", coreId "(=)", AFunctionDefinition [
                makeInternalFn [Reference rtKindInt, rtKindInt] rtKindInt "(=)RII"])

-- fixme this should be in its own file, and grab in stuff from elsewhere to!
standardInternalFunctions :: InternalFunctions KStat s
standardInternalFunctions =
    Map.fromList [
            ("(+)II",
              \ [Left (KindInt a),Left (KindInt b)] ->
                  return $ Left $ KindInt (a+b)),
            ("(=)RII",
              \ [Right ref, Left c] -> kstatWriteRef ref c >>
                                       return (Left c))
    ]
