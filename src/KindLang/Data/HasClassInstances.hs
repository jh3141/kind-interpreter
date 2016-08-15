{-# LANGUAGE DataKinds,FlexibleInstances,MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts,TypeFamilies,UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes,ScopedTypeVariables #-}

-- | This module should be updated for any types that may need to be stored
-- in a 'Value' "box".
module KindLang.Data.HasClassInstances where

import Data.Type.HasClass
import KindLang.Data.AST
import KindLang.Data.BasicTypes
import KindLang.Data.Error

instance HasClass Eq AExpr True where classDict _ _ _ = TDict
instance HasClass Show AExpr True where classDict _ _ _ = TDict
instance HasClass Eq AStatement True where classDict _ _ _ = TDict
instance HasClass Show AStatement True where classDict _ _ _ = TDict
instance HasClass Eq ClassMember True where classDict _ _ _ = TDict
instance HasClass Show ClassMember True where classDict _ _ _ = TDict
instance HasClass Eq Definition True where classDict _ _ _ = TDict
instance HasClass Show Definition True where classDict _ _ _ = TDict
instance HasClass Eq DefList True where classDict _ _ _ = TDict
instance HasClass Show DefList True where classDict _ _ _ = TDict
instance HasClass Eq Expr True where classDict _ _ _ = TDict
instance HasClass Show Expr True where classDict _ _ _ = TDict
instance HasClass Eq ExprAnnotation True where classDict _ _ _ = TDict
instance HasClass Show ExprAnnotation True where classDict _ _ _ = TDict
instance HasClass Eq FunctionInstance True where classDict _ _ _ = TDict
instance HasClass Show FunctionInstance True where classDict _ _ _ = TDict
instance HasClass Eq IdentDefinition True where classDict _ _ _ = TDict
instance HasClass Show IdentDefinition True where classDict _ _ _ = TDict
instance HasClass Eq Module True where classDict _ _ _ = TDict
instance HasClass Show Module True where classDict _ _ _ = TDict
instance HasClass Eq ModuleImport True where classDict _ _ _ = TDict
instance HasClass Show ModuleImport True where classDict _ _ _ = TDict
instance HasClass Eq Statement True where classDict _ _ _ = TDict
instance HasClass Show Statement True where classDict _ _ _ = TDict
instance HasClass Eq StmtAnnotation True where classDict _ _ _ = TDict
instance HasClass Show StmtAnnotation True where classDict _ _ _ = TDict
instance HasClass Eq TypeDescriptor True where classDict _ _ _ = TDict
instance HasClass Show TypeDescriptor True where classDict _ _ _ = TDict
instance HasClass Eq TypePredicate True where classDict _ _ _ = TDict
instance HasClass Show TypePredicate True where classDict _ _ _ = TDict
instance HasClass Eq VariableInitializer True where classDict _ _ _ = TDict
instance HasClass Show VariableInitializer True where classDict _ _ _ = TDict
instance HasClass Eq Visibility True where classDict _ _ _ = TDict
instance HasClass Show Visibility True where classDict _ _ _ = TDict
