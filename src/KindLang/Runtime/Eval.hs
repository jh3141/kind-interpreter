module KindLang.Runtime.Eval where

import qualified Data.Map as Map
import Control.Monad.ST.Trans
import Control.Monad.Except
import KindLang.Data.BasicTypes
import KindLang.Data.Value
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.Scope
import KindLang.Locale.ErrorMessages
    
type InternalFunctions = Map.Map InternalFunctionName ([Value] -> Value)
type RunM s a = STT s (Except KindError) a
    
data RuntimeScope s = RuntimeScope {
      rtsStateIndex  :: Map.Map NSID (STRef s Value),
      rtsScope       :: Scope,
      rtsParent      :: Maybe (RuntimeScope s)
     }

newRuntimeScope :: Scope -> RuntimeScope s
newRuntimeScope sc = RuntimeScope Map.empty sc Nothing

makeChildRuntimeScope :: RuntimeScope s -> Scope -> RuntimeScope s
makeChildRuntimeScope p@(RuntimeScope si ps pp) sc
    | scopeParent sc == Just ps = RuntimeScope si sc (Just p)
    | otherwise                 = error
                                  "scope has incorrect parent in makeChildRuntimeScope"

rtsLookupRef :: RuntimeScope s -> NSID -> RunM s (STRef s Value)
rtsLookupRef (RuntimeScope index _ _) i =
    maybe
      (throwError $ InternalError $ "runtime reference not found for " ++ nsidString i)
      return
      (Map.lookup i index)

runtimeScopeAddItems :: RuntimeScope s -> [(NSID,Value)] -> RunM s (RuntimeScope s)
runtimeScopeAddItems scope values = foldM runtimeScopeAddItem scope values

runtimeScopeAddItem :: RuntimeScope s -> (NSID,Value) -> RunM s (RuntimeScope s)
runtimeScopeAddItem (RuntimeScope idx sc p) (sid,val) = do
    stref <- newSTRef val
    return $ RuntimeScope (Map.insert sid stref idx) sc p
           
kerrToRun :: KErr a -> RunM s a
kerrToRun e = either throwError return $ runExcept e

-- apparently this needs to have rank-2 type to work.  if it gets any more complex, I'd
-- recommend it, but for now just use runST directly.
--runToKErr :: RunM s a -> KErr a
--runToKErr r = runST r
              
-- fixme: going to need access to program mutable state, and ability to mutate it!
evalAExpr :: RuntimeScope s -> InternalFunctions -> AExpr -> RunM s Value
evalAExpr _ _ (AIntLiteral _ val) = return $ makeKindInt val
evalAExpr s ifc (AFunctionApplication _ efn eargs) = do
    fn <- evalAExpr s ifc efn
    args <- mapM (evalAExpr s ifc) eargs
    applyFunction s ifc (getKindFunctionRef fn) args
evalAExpr s _ (AVarRef ae id) =
    -- fixme (performance) - do we need to perform the scope lookup if we already
    -- have an annotated expression including the canonical id?  can we not 
    -- simply shortcircuit lookup at this point?
    kerrToRun (snd <$> scopeLookup (rtsScope s) id) >>= (definitionToValue s ae) 

-- fixme would it be more efficient to use starrays and references into them than
-- individal strefs for each defined variable?

definitionToValue :: RuntimeScope s -> ExprAnnotation -> Definition -> RunM s Value
definitionToValue _ _ (FunctionDefinition insts) = return $ makeKindFunctionRef insts
definitionToValue s ea (VariableDefinition _ _) = do
    canonicalID <- kerrToRun $ errorIfNothing (exprAnnotationCanonicalID ea)
                                              (InternalError requiredCanonicalID)
    stref <- rtsLookupRef s canonicalID
    readSTRef stref

applyFunction :: RuntimeScope s -> InternalFunctions -> [FunctionInstance] -> [Value] ->
                 RunM s Value
applyFunction s ifc (inst:[]) v = applyFunctionInstance s ifc inst v
-- fixme handle overloaded functions

applyFunctionInstance :: RuntimeScope s -> InternalFunctions -> FunctionInstance ->
                         [Value] -> RunM s Value
applyFunctionInstance _ ifc (InternalFunction _ n) vs =
    maybe (throwError $ InternalError $ "Unknown internal function " ++ n) -- if Nothing
          (\f -> return $ f vs)                                            -- if Just f
          (Map.lookup n ifc)
applyFunctionInstance s ifc (AFunctionInstance _ formal stmt) actual =
    -- fixme apply parameter values to scope/mutable state
    evalAStatement s ifc stmt
-- fixme on-demand function body type resolution

evalAStatement :: RuntimeScope s -> InternalFunctions -> AStatement -> RunM s Value
evalAStatement s ifc (AExpression _ e) = evalAExpr s ifc e
                                         
