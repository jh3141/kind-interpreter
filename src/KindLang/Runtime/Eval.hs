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

scopeAddItemWithDef :: Scope s -> (String,Value,Definition) -> KStat s ()
scopeAddItemWithDef sc (lid,val,def) = 
    scopeAddItem sc (UnqualifiedID lid, definitionToType def, val)

-- fixme: going to need access to program mutable state, and ability to mutate it!
evalAExpr :: Scope s -> InternalFunctions -> AExpr -> KStat s Value
evalAExpr _ _ (AIntLiteral _ val) = return $ makeKindInt val
evalAExpr _ _ (AStringLiteral _ val) = return $ makeKindString val
evalAExpr s ifc (AFunctionApplication _ efn eargs) = do
    fn <- evalAExpr s ifc efn
    args <- mapM (evalAExpr s ifc) eargs
    applyFunction s ifc (getKindFunctionRef fn) (aexprType <$> eargs) args
evalAExpr s ifc (AVarRef ae id) =
    snd <$> scopeLookup s id >>= definitionToValue s ifc ae
evalAExpr _ _ expr = throwError $ InternalError
                     ("attempted to evaluate unimplemented expression: " ++
                      show expr)
-- fixme would it be more efficient to use starrays and references into them than
-- individal strefs for each defined variable?

-- | A scope lookup may return either a definition or a value.  If we always
-- want a value, we can convert the definition to a value using this function.
definitionToValue :: Scope s -> InternalFunctions -> ExprAnnotation ->
                     DefinitionOrValue -> KStat s Value
definitionToValue _ _ _ (Left (FunctionDefinition insts)) =
    return $ makeKindFunctionRef insts
definitionToValue s ifc ea (Left (VariableDefinition _ _)) = do
    -- fixme should we lazy-initialize at this point?
    canonicalID <- errorIfNothing (exprAnnotationCanonicalID ea)
                                  (InternalError requiredCanonicalID)
    stref <- scopeLookupRef s canonicalID (initializeItem s ifc)
    kstatReadRef stref
definitionToValue _ _ _ (Right (_,v)) = return v  -- was already a value

applyFunction :: Scope s -> InternalFunctions -> [FunctionInstance] ->
                 [TypeDescriptor] -> [Value] -> KStat s Value
applyFunction s ifc (inst:[]) _ v = applyFunctionInstance s ifc inst v
applyFunction s ifc (i:is) vTypes v
              | fnInstCompatible i vTypes = applyFunctionInstance s ifc i v
              | otherwise                 = applyFunction s ifc is vTypes v

applyFunctionInstance :: Scope s -> InternalFunctions ->
                         FunctionInstance -> [Value] -> KStat s Value
applyFunctionInstance _ ifc (InternalFunction _ n) vs =
    maybe (unknownInstance)      -- if Nothing
          (\f -> return $ f vs)  -- if Just f
          (Map.lookup n ifc)
    where
      unknownInstance = throwError $ InternalError $
                        "Unknown internal function " ++ n
applyFunctionInstance s ifc (AFunctionInstance td@(FunctionType formalTypes _)
                                               formal stmt) actual = do
    functionScope <- makeFunctionScope s td formal
    scopeAddItems functionScope (zip3 (UnqualifiedID <$> formal)
                                      formalTypes
                                      actual)
    evalAStatement functionScope ifc stmt

applyFunctionInstance _ _ (FunctionInstance _ _ _) _ =
    throwError $ InternalError
                 "Function instances should be resolved before application"

-- fixme on-demand function body type resolution

evalAStatement :: Scope s -> InternalFunctions -> AStatement ->
                  KStat s Value  -- nb may alter scope
evalAStatement s ifc (AExpression _ e) =  evalAExpr s ifc e
evalAStatement s ifc (AVarDeclStatement _ lid td varInit) = do
    defaultValue <- evaluateVarInit s ifc td varInit
    scopeAddItemWithDef s
                       (lid, defaultValue, VariableDefinition td VarInitNone)
    return KindUnit
evalAStatement s ifc (AStatementBlock _ stmts) =
    last <$> mapM (evalAStatement s ifc) stmts

evaluateVarInit :: Scope s -> InternalFunctions -> TypeDescriptor ->
                   VariableInitializer -> KStat s Value
evaluateVarInit s ifc td VarInitNone = defaultValueOfType s ifc td
evaluateVarInit s ifc td (VarInitAExpr e) = evalAExpr s ifc e
evaluateVarInit _ _ _ (VarInitExpr _) = throwError $
                                        InternalError "Unresolved expression in varinit"
-- fixme other init types

defaultValueOfType :: Scope s -> InternalFunctions -> TypeDescriptor ->
                      KStat s Value
defaultValueOfType _ _ (ResolvedType _ (QualifiedID "kind" (UnqualifiedID "int")) _) =
    return $ makeKindInt 0
defaultValueOfType _ _ t = throwError $ InternalError $
                       "No default value defined for type " ++ show t

-- FIXME shouldn't initialization occur in the scope in which the definition
-- was written?
initializeItem :: Scope s -> InternalFunctions -> Definition ->
                  KStat s (TypeDescriptor, Value)
initializeItem s ifc (VariableDefinition td varInit) =
    (td,) <$> evaluateVarInit s ifc td varInit
-- fixme what about other definition types?

-- | Force definition of a variable/constant for any appropriate items in
-- the given scope that are currently stored as definitions
instantiateScopeDefinitions :: Scope s -> KStat s ()
instantiateScopeDefinitions sc =
    scopeItems sc >>= mapM_ (instantiateDefinition sc)

instantiateDefinition :: Scope s -> (NSID, NSID, DefinitionOrValue) ->
                         KStat s ()
instantiateDefinition s (rid, cid, Left def) =
    case makeDefinitionValue def of
      Nothing  -> return ()
      Just val -> initializeRef (return . const val) s rid cid def >> return ()
instantiateDefinition s (rid, nsid, _) = return ()

makeDefinitionValue :: Definition -> Maybe (TypeDescriptor,Value)
makeDefinitionValue (FunctionDefinition (inst:[])) =
    Just (fnInstanceType inst, makeKindFunctionRef [inst]) -- fixme overloads
makeDefinitionValue (ClassDefinition members) = Nothing
makeDefinitionValue _ = Nothing
