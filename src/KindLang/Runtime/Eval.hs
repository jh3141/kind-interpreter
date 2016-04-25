module KindLang.Runtime.Eval where

import qualified Data.Map as Map
import Control.Monad.ST.Trans
import Control.Monad.Except
import KindLang.Data.Value
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.Scope

type InternalFunctions = Map.Map InternalFunctionName ([Value] -> Value)
type RunM s a = STT s (Except KindError) a

kerrToRun :: KErr a -> RunM s a
kerrToRun e = either throwError return $ runExcept e

--runToKErr :: RunM s a -> KErr a
--runToKErr r = either throwError return $ runExcept $ runST r
              
-- fixme: going to need access to program mutable state, and ability to mutate it!
evalAExpr :: Scope -> InternalFunctions -> AExpr -> RunM s Value
evalAExpr _ _ (AIntLiteral _ val) = return $ makeKindInt val
evalAExpr s ifc (AFunctionApplication _ efn eargs) = do
    fn <- evalAExpr s ifc efn
    args <- mapM (evalAExpr s ifc) eargs
    applyFunction s ifc (getKindFunctionRef fn) args
evalAExpr s _ (AVarRef ae id) =
    kerrToRun (snd <$> scopeLookup s id) -- fixme handle vars, not just constants.
      >>= (definitionToValue ae) 

definitionToValue :: ExprAnnotation -> Definition -> RunM s Value
definitionToValue _ (FunctionDefinition insts) = return $ makeKindFunctionRef insts

applyFunction :: Scope -> InternalFunctions -> [FunctionInstance] -> [Value] ->
                 RunM s Value
applyFunction s ifc (inst:[]) v = applyFunctionInstance s ifc inst v
-- fixme handle overloaded functions

applyFunctionInstance :: Scope -> InternalFunctions -> FunctionInstance -> [Value] ->
                         RunM s Value
applyFunctionInstance _ ifc (InternalFunction _ n) vs =
    maybe (throwError $ InternalError $ "Unknown internal function " ++ n) -- if Nothing
          (\f -> return $ f vs)                                            -- if Just f
          (Map.lookup n ifc)
applyFunctionInstance s ifc (AFunctionInstance _ formal stmt) actual =
    -- fixme apply parameter values to scope/mutable state
    evalAStatement s ifc stmt
-- fixme on-demand function body type resolution

evalAStatement :: Scope -> InternalFunctions -> AStatement -> RunM s Value
evalAStatement s ifc (AExpression _ e) = evalAExpr s ifc e
