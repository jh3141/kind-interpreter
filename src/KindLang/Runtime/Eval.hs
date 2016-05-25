{-# LANGUAGE TupleSections #-}

module KindLang.Runtime.Eval where
import Debug.Trace
import Data.List
import Data.Foldable

import qualified Data.Map as Map
import Data.STRef
import Control.Monad.Except
import KindLang.Data.BasicTypes
import KindLang.Data.Value
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.Scope
import KindLang.Data.KStat
import KindLang.Data.Types
import KindLang.Locale.ErrorMessages

type InternalFunctions = Map.Map InternalFunctionName ([Value] -> Value)

data RuntimeScope s = RuntimeScope {
      rtsStateIndex  :: Map.Map NSID (STRef s Value),
      rtsScope       :: Scope s,
      rtsParent      :: Maybe (RuntimeScope s)
     }

newRuntimeScope :: Scope s -> RuntimeScope s
newRuntimeScope sc = RuntimeScope Map.empty sc Nothing

makeChildRuntimeScope :: RuntimeScope s -> Scope s -> RuntimeScope s
makeChildRuntimeScope p@(RuntimeScope si ps pp) sc
    | scopeParent sc == Just ps = RuntimeScope si sc (Just p)
    | otherwise                 = error
                                  "scope has incorrect parent in makeChildRuntimeScope"

rtsLookupRef :: RuntimeScope s -> NSID -> KStat s (STRef s Value)
rtsLookupRef (RuntimeScope index _ _) i =
    maybe
      (throwError $ InternalError $ "runtime reference not found for " ++ nsidString i)
      return
      (Map.lookup i index)

runtimeScopeAddItems :: RuntimeScope s -> [(NSID,Value)] -> KStat s (RuntimeScope s)
runtimeScopeAddItems scope values = foldM runtimeScopeAddItem scope values

runtimeScopeAddItem :: RuntimeScope s -> (NSID,Value) -> KStat s (RuntimeScope s)
runtimeScopeAddItem (RuntimeScope idx sc p) (sid,val) = do
    stref <- kstatNewRef val
    return $ RuntimeScope (Map.insert sid stref idx) sc p

runtimeScopeAddItemWithDef :: RuntimeScope s -> (String,Value,Definition)
                           -> KStat s (RuntimeScope s)
runtimeScopeAddItemWithDef rts1 (lid,val,def) = do
    (RuntimeScope idx sc p) <- runtimeScopeAddItem rts1 (UnqualifiedID lid,val)
    return $ RuntimeScope idx (sc |@+| (lid,def)) p


-- fixme: going to need access to program mutable state, and ability to mutate it!
evalAExpr :: RuntimeScope s -> InternalFunctions -> AExpr -> KStat s Value
evalAExpr _ _ (AIntLiteral _ val) = return $ makeKindInt val
evalAExpr s ifc (AFunctionApplication _ efn eargs) = do
    fn <- evalAExpr s ifc efn
    args <- mapM (evalAExpr s ifc) eargs
    applyFunction s ifc (getKindFunctionRef fn) args
evalAExpr s _ (AVarRef ae id) =
    -- fixme (performance) - do we need to perform the scope lookup if we already
    -- have an annotated expression including the canonical id?  can we not 
    -- simply shortcircuit lookup at this point?
    snd <$> scopeLookup (rtsScope s) id >>= definitionToValue s ae
evalAExpr s ifns (AInternalRef (ExprAnnotation td _) name) = do
    name <- errorIfNothing (lookupOverloadedInternalFunctionName name td ifns)
                           (NoAppropriateInstance name td)
    return (KindFunctionRef [InternalFunction td name])
evalAExpr _ _ expr = throwError $ InternalError ("attempted to evaluate unimplemented expression: " ++ show expr)
-- fixme would it be more efficient to use starrays and references into them than
-- individal strefs for each defined variable?

definitionToValue :: RuntimeScope s -> ExprAnnotation -> Definition -> KStat s Value
definitionToValue _ _ (FunctionDefinition insts) = return $ makeKindFunctionRef insts
definitionToValue s ea (VariableDefinition _ _) = do
    canonicalID <- errorIfNothing (exprAnnotationCanonicalID ea)
                                  (InternalError requiredCanonicalID)
    stref <- rtsLookupRef s canonicalID
    kstatReadRef stref

applyFunction :: RuntimeScope s -> InternalFunctions -> [FunctionInstance] -> [Value] ->
                 KStat s Value
applyFunction s ifc (inst:[]) v = applyFunctionInstance s ifc inst v
-- fixme handle overloaded functions

applyFunctionInstance :: RuntimeScope s -> InternalFunctions -> FunctionInstance ->
                         [Value] -> KStat s Value
applyFunctionInstance _ ifc (InternalFunction _ n) vs =
    maybe (throwError $ InternalError $ "Unknown internal function " ++ n) -- if Nothing
          (\f -> return $ f vs)                                            -- if Just f
          (Map.lookup n ifc)
applyFunctionInstance s ifc (AFunctionInstance td formal stmt) actual = do
    childScope <- runtimeScopeAddItems
                      (makeChildRuntimeScope s
                       (makeFunctionScope (rtsScope s) td formal))
                      (zip (UnqualifiedID <$> formal) actual)
    (_, val) <- evalAStatement childScope ifc stmt
    return $ val
applyFunctionInstance _ _ (FunctionInstance _ _ _) _ =
    throwError $ InternalError
                 "Function instances should be resolved before application"

-- fixme on-demand function body type resolution

evalAStatement :: RuntimeScope s -> InternalFunctions -> AStatement ->
                  KStat s (RuntimeScope s, Value)
evalAStatement s ifc (AExpression _ e) = (s,) <$> evalAExpr s ifc e
evalAStatement s ifc (AVarDeclStatement _ lid td varInit) = do
    defaultValue <- evaluateVarInit s ifc td varInit
    (,KindUnit) <$> runtimeScopeAddItemWithDef s
                       (lid, defaultValue, VariableDefinition td VarInitNone)
evalAStatement s ifc (AStatementBlock _ stmts) =
    foldM (flip evalAStatement ifc . traceRTS . fst) (s, KindUnit) stmts

traceRTS :: RuntimeScope s -> RuntimeScope s
traceRTS arg@(RuntimeScope index scope _) =
    trace ("Index: " ++ (show $ Map.keys index))
    trace ("Scope: " ++ (show $ Map.keys $ scopeCat scope))
    arg

evaluateVarInit :: RuntimeScope s -> InternalFunctions -> TypeDescriptor ->
                   VariableInitializer -> KStat s Value
evaluateVarInit s ifc td VarInitNone = defaultValueOfType s ifc td
evaluateVarInit s ifc td (VarInitAExpr e) = evalAExpr s ifc e
evaluateVarInit _ _ _ (VarInitExpr _) = throwError $
                                        InternalError "Unresolved expression in varinit"
-- fixme other init types

defaultValueOfType :: RuntimeScope s -> InternalFunctions -> TypeDescriptor ->
                      KStat s Value
defaultValueOfType _ _ (ResolvedType _ (QualifiedID "kind" (UnqualifiedID "int")) _) =
    return $ makeKindInt 0
defaultValueOfType _ _ t = throwError $ InternalError $
                       "No default value defined for type " ++ show t

lookupOverloadedInternalFunctionName :: NSID -> TypeDescriptor -> InternalFunctions ->
                                        Maybe String
lookupOverloadedInternalFunctionName (QualifiedID "kind" sid) td ifns =
    lookupOverloadedInternalFunctionName sid td ifns
lookupOverloadedInternalFunctionName sid (FunctionType args rtype) ifns =
    find ((flip Map.member) ifns) $
         map buildName (candidateFunctionCallArgTypes args)
    where
      buildName targs = nsidString sid ++ " (" ++
                        (intercalate "," (typeName <$> targs)) ++ ")"

