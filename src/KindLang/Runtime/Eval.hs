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

-- fixme - had to remove check that scopeParent sc == Just ps because
-- scope is no longer an instance of Eq; find another way of doing this
-- check. if we still need it. (previously just gave an error if not equal)

makeChildRuntimeScope :: RuntimeScope s -> Scope s -> RuntimeScope s
makeChildRuntimeScope p@(RuntimeScope si ps pp) sc = RuntimeScope si sc (Just p)

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
    newScope <- scopeUpdate sc |@+| (lid,def)
    return $ RuntimeScope idx newScope p

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

-- | A scope lookup may return either a definition or a value.  If we always
-- want a value, we can convert the definition to a value using this function.
definitionToValue :: RuntimeScope s -> ExprAnnotation -> DefinitionOrValue ->
                     KStat s Value
definitionToValue _ _ (Left (FunctionDefinition insts)) =
    return $ makeKindFunctionRef insts
definitionToValue s ea (Left (VariableDefinition _ _)) = do
    -- fixme should we lazy-initialize at this point?
    canonicalID <- errorIfNothing (exprAnnotationCanonicalID ea)
                                  (InternalError requiredCanonicalID)
    stref <- rtsLookupRef s canonicalID
    kstatReadRef stref
definitionToValue _ _ (Right (_,v)) = return v  -- was already a value

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
    functionScope <- makeFunctionScope (rtsScope s) td formal
    childScope <- runtimeScopeAddItems
                      (makeChildRuntimeScope s functionScope)
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
    foldM (flip evalAStatement ifc . fst) (s, KindUnit) stmts

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

