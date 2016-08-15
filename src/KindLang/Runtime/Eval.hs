{-# LANGUAGE TupleSections #-}

module KindLang.Runtime.Eval where
import Debug.Trace
import Data.List
import Data.Foldable

import qualified Data.Map as Map
import Data.STRef
import Data.Array.ST
import Control.Monad.Except
import KindLang.Data.BasicTypes
import KindLang.Data.Value
import KindLang.Data.AST
import KindLang.Data.Error
import KindLang.Data.Scope
import KindLang.Data.KStat
import KindLang.Data.MStat
import KindLang.Data.Types
import KindLang.Locale.ErrorMessages
import KindLang.Analysis.ResolveTypes
import KindLang.Lib.InternalFunctions
import KindLang.Runtime.Data
import KindLang.Runtime.Metaclass

scopeAddItemWithDef :: Scope s -> (String,Value s,Definition) -> KStat s ()
scopeAddItemWithDef sc (lid,val,def) = 
    scopeAddItem sc (UnqualifiedID lid, definitionToType def, val)

evalAExpr :: Scope s -> AExpr -> KStat s (ValueOrRef s)
evalAExpr _ (AIntLiteral _ val) = return $ Left $ makeKindInt val
evalAExpr _ (AStringLiteral _ val) = return $ Left $ makeKindString val
evalAExpr s (AFunctionApplication _ efn eargs) = do
    fn <- evalAExpr s efn >>= refToValue
    args <- mapM (evalAExpr s) eargs
    applyFunction s (getKindFunctionRef fn) (aexprType <$> eargs) args
evalAExpr s (AVarRef ae id) =
    Right <$> snd <$> scopeLookupRef s id initializeItem
evalAExpr _ expr = throwError $ InternalError
                     ("attempted to evaluate unimplemented expression: " ++
                      show expr)
-- fixme would it be more efficient to use starrays and references into them than
-- individal strefs for each defined variable?

refToValue :: ValueOrRef s -> KStat s (Value s)
refToValue (Left v) = return v
refToValue (Right ref) = kstatReadRef ref

applyFunction :: Scope s -> [FunctionInstance] ->
                 [TypeDescriptor] -> [ValueOrRef s] -> KStat s (ValueOrRef s)
applyFunction s (inst:[]) _ v = applyFunctionInstance s inst v
applyFunction s (i:is) vTypes v
              | fnInstCompatible i vTypes =
                            do
                              i' <- substituteInferableTypes s i vTypes
                              applyFunctionInstance s i' v
              | otherwise = applyFunction s is vTypes v

-- fixme this should probably be in another module
-- fixme the scope here should be the function's outer scope. how do we find that?
substituteInferableTypes :: Scope s -> FunctionInstance -> [TypeDescriptor] ->
                            KStat s FunctionInstance
substituteInferableTypes _ f@(InternalFunction _ _) _ = return f
substituteInferableTypes sc f@(AFunctionInstance td@(FunctionType pt _) _ _) at
    | InferableType `notElem` pt = return f
    | otherwise =  substituteInferableTypes sc (stripFnInstanceAnnotations f) at
substituteInferableTypes sc (FunctionInstance td@(FunctionType pt rt) f st) at =
    resolveInstance sc (FunctionInstance updatedType f st)
    where
      updatedType = FunctionType (zipWith specializeType pt at) rt
      specializeType InferableType a = a
      specializeType p _             = p

applyFunctionInstance :: Scope s -> FunctionInstance -> [ValueOrRef s] ->
                         KStat s (ValueOrRef s)
applyFunctionInstance _ (InternalFunction (FunctionType ts _) n) vs =
    join (kstatInternalFunctionLookup n <*> dereferenceArgList vs ts)

applyFunctionInstance s (AFunctionInstance td@(FunctionType formalTypes _)
                                               formal stmt) actual = do
    functionScope <- makeFunctionScope s td formal
    actualValues <- mapM refToValue actual
    scopeAddItems functionScope (zip3 (UnqualifiedID <$> formal)
                                      formalTypes
                                      actualValues) -- fixme ref args
    Left <$> evalAStatement functionScope stmt -- fixme how to return refs?

applyFunctionInstance _ (FunctionInstance _ _ _) _ =
    throwError $ InternalError
                 "Function instances should be resolved before application"

dereferenceArgList :: [ValueOrRef s] -> [TypeDescriptor] ->
                      KStat s [ValueOrRef s]
dereferenceArgList vs ts = mapM (uncurry dereferenceIfRequired) (zip vs ts)

dereferenceIfRequired :: ValueOrRef s -> TypeDescriptor -> KStat s (ValueOrRef s)
dereferenceIfRequired (Left v)  _             = return $ Left v
dereferenceIfRequired (Right r) (Reference _) = return $ Right r
dereferenceIfRequired (Right r) _             = Left <$> kstatReadRef r

-- fixme on-demand function body type resolution

evalAStatement :: Scope s -> AStatement ->
                  KStat s (Value s)  -- nb may alter scope
evalAStatement s (AExpression _ e) =  evalAExpr s e >>= refToValue
evalAStatement s (AVarDeclStatement _ lid td varInit) = do
    defaultValue <- evaluateVarInit s td varInit
    scopeAddItemWithDef s
                       (lid, defaultValue, VariableDefinition td VarInitNone)
    return KindUnit
evalAStatement s (AStatementBlock _ stmts) =
    last <$> mapM (evalAStatement s) stmts

evaluateVarInit :: Scope s -> TypeDescriptor ->
                   VariableInitializer -> KStat s (Value s)
evaluateVarInit s td VarInitNone = defaultValueOfType s td
evaluateVarInit s  td (VarInitAExpr e) = evalAExpr s e >>= refToValue
evaluateVarInit _ _ (VarInitExpr _) = throwError $
                                      InternalError "Unresolved expression in varinit"
-- fixme other init types

defaultValueOfType :: Scope s -> TypeDescriptor ->
                      KStat s (Value s)
defaultValueOfType _ (ResolvedType _ (QualifiedID "kind" (UnqualifiedID "int")) _) =
    return $ makeKindInt 0
defaultValueOfType _ t = throwError $ InternalError $
                       "No default value defined for type " ++ show t

-- FIXME shouldn't initialization occur in the scope in which the definition
-- was written?
initializeItem :: Scope s -> Definition -> KStat s (TypeDescriptor, Value s)
initializeItem s (VariableDefinition td varInit) =
    (td,) <$> evaluateVarInit s td varInit
initializeItem _ (FunctionDefinition insts) =
    return $ (makeFunctionType insts, makeKindFunctionRef insts)
initializeItem s def@(ClassDefinition _) = do
    metaClassRef <- getKindDefaultMetaclass s
    members <- liftToST $ newListArray (0, 0) [
       makeKindBox def
       ]
    return (rtDefaultMetaclass, KindObject metaClassRef members)
-- fixme what about other definition types?

-- | Force definition of a variable/constant for any appropriate items in
-- the given scope that are currently stored as definitions
instantiateScopeDefinitions :: Scope s -> KStat s ()
instantiateScopeDefinitions sc =
    scopeItems sc >>= mapM_ (instantiateDefinition sc)

instantiateDefinition :: Scope s -> (NSID, NSID, DefinitionOrValue s) ->
                         KStat s ()
instantiateDefinition s (rid, cid, Left def) =
    case makeDefinitionValue def of
      Nothing  -> return ()
      Just val -> initializeRef (\_ -> return . const val) s rid cid def >>
                  return ()
instantiateDefinition s (rid, nsid, _) = return ()

makeDefinitionValue :: Definition -> Maybe (TypeDescriptor,Value s)
makeDefinitionValue (FunctionDefinition (inst:[])) =
    Just (fnInstanceType inst, makeKindFunctionRef [inst]) -- fixme overloads
makeDefinitionValue (ClassDefinition members) = Nothing
makeDefinitionValue _ = Nothing
