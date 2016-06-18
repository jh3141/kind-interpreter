module KindLang.Data.Types where

import Data.Foldable
import Data.List
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Error

-- fixme these functions probably belong in a module for type expressions
generateSubstitution :: TypeDescriptor -> TypeDescriptor ->
                        Maybe (String, TypeDescriptor)
-- don't propagate reference nature into inferred type variable bindings
generateSubstitution (TypeVariable name) (Reference act) = Just (name, act)
generateSubstitution (TypeVariable name) act = Just (name, act)
generateSubstitution _ _ = Nothing

substituteTypeVar :: String -> TypeDescriptor -> TypeDescriptor ->
                     TypeDescriptor
substituteTypeVar name value (TypeVariable x) | x == name = value
                                              | otherwise = (TypeVariable x)
substituteTypeVar name value (FunctionType args ret) =
    FunctionType ((substituteTypeVar name value) <$> args)
                 (substituteTypeVar name value ret)
substituteTypeVar name value (Reference td) =
    Reference (substituteTypeVar name value td)
substituteTypeVar _ _ td = td   -- fixme other types that need substituting?

-- | @typesCompatible f a@ determines whether a function whose formal parameters
-- have types @f@ can be invoked with actual parameters of type @a@, returning
-- True or False.
typesCompatible :: [TypeDescriptor] -> [TypeDescriptor] -> Bool
typesCompatible tdl1 tdl2 = and $ map (uncurry typeCompatible) $ zip tdl1 tdl2

-- | @typeCompatible td td'@ returns True iff a value of type @td'@ can be
-- used in a situation where the expected type is @td@
typeCompatible :: TypeDescriptor -> TypeDescriptor -> Bool
-- inferable type objects can hold whatever type we determine the code will
-- put in them
typeCompatible InferableType _ = True
-- reference types are compatible if their referee types are compatible
typeCompatible (Reference t1) (Reference t2) = typeCompatible t1 t2
-- we can implicitly convert references to their referees
typeCompatible t1 (Reference t2) = typeCompatible t1 t2
-- otherwise, we expect types to be the same...
typeCompatible x y = x == y   -- fixme - subtypes?

-- fixme document this
typeName :: TypeDescriptor -> String
typeName (SimpleType sid) = nsidString sid
typeName (ResolvedType _ sid _) = nsidString sid
typeName (FunctionType args rtype) = "(" ++ (intercalate "," (typeName <$> args))
                                     ++ ")->" ++ (typeName rtype)
typeName (ForAllTypes names preds td) = "[" ++ (intercalate "," names) ++ "]" ++
                                        typeName td
typeName (TypeVariable n) = n
typeName (SumType tds) = "(" ++ (intercalate "|" (typeName <$> tds)) ++ ")"
typeName (TupleType tds) = "(" ++ (intercalate "," (typeName <$> tds)) ++ ")"
typeName InferableType = "<inferred>"
typeName (RecordType rid tds) = nsidString rid ++ "{" ++
                                (intercalate "," (typeName <$> tds)) ++ "}"
typeName (Reference td) = "&" ++ typeName td


candidateFunctionCallArgTypes :: [TypeDescriptor] -> [[TypeDescriptor]]
candidateFunctionCallArgTypes args = foldrM prependAlternatives []
                                            (map getAlternativeTypes args)

getAlternativeTypes :: TypeDescriptor -> [TypeDescriptor]
getAlternativeTypes (Reference t) = [Reference t, t]
getAlternativeTypes t = [t]

prependAlternatives :: [a] -> [a] -> [[a]]
prependAlternatives x l = map (\z -> z : l) x

definitionToType :: Definition -> TypeDescriptor
definitionToType (ClassDefinition members) = undefined -- FIXME metaclass!
definitionToType (FunctionDefinition (inst:[])) = fnInstanceType inst
definitionToType (FunctionDefinition _) = undefined    -- FIXME overloading!
definitionToType (VariableDefinition td _) = td
definitionToType (Namespace _) = error "Namespaces do not have types"
definitionToType (InternalObject td) = td

typeResolved :: TypeDescriptor -> Bool
typeResolved (SimpleType _) = False
typeResolved (Reference td) = typeResolved td
typeResolved _              = True

fnInstCompatible :: FunctionInstance -> [TypeDescriptor] -> Bool
fnInstCompatible = fnTypeCompatible . fnInstanceType

fnTypeCompatible :: TypeDescriptor -> [TypeDescriptor] -> Bool
fnTypeCompatible (FunctionType argTypes _) paramTypes =
    checkAll argTypes paramTypes
    where
      checkAll [] [] = True
      checkAll _ [] = False
      checkAll [] _ = False
      checkAll (a:as) (b:bs) = typeCompatible a b && checkAll as bs
