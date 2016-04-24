module KindLang.Runtime.Eval where

import qualified Data.Map as Map
import KindLang.Data.Value
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.Scope

type InternalFunctions = Map.Map InternalFunctionName ([Value] -> Value)
    
-- fixme: going to need access to program mutable state, and ability to mutate it!
evalAExpr :: Scope -> InternalFunctions -> AExpr -> KErr Value
evalAExpr _ _ (AIntLiteral _ val) = return $ makeKindInt val
evalAExpr s ifc (AFunctionApplication _ efn eargs) = do
    fn <- evalAExpr s ifc efn
    args <- mapM (evalAExpr s ifc) eargs
    applyFunction s ifc (getKindFunctionRef fn) args
evalAExpr s _ (AVarRef ae id) = (definitionToValue ae) <$> snd <$> scopeLookup s id -- fixme handle vars, not just constants.

definitionToValue :: ExprAnnotation -> Definition -> Value
definitionToValue _ (FunctionDefinition insts) = makeKindFunctionRef insts

applyFunction :: Scope -> InternalFunctions -> [FunctionInstance] -> [Value] ->
                 KErr Value
applyFunction s ifc (inst:[]) v = applyFunctionInstance s ifc inst v
-- fixme handle overloaded functions

applyFunctionInstance :: Scope -> InternalFunctions -> FunctionInstance -> [Value] ->
                         KErr Value
applyFunctionInstance _ ifc (InternalFunction _ n) vs =
    maybe (Left $ InternalError $ "Unknown internal function " ++ n) -- if Nothing
          (\f -> Right $ f vs)                                       -- if Just f
          (Map.lookup n ifc)
-- fixme non-native functions

