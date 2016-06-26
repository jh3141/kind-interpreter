{-# LANGUAGE FlexibleContexts #-}
module KindLang.Runtime.Metaclass where

import Control.Monad.Except
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import KindLang.Runtime.Data
import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Data.Value
import KindLang.Data.Scope
import KindLang.Data.MStat
import KindLang.Data.Error
import KindLang.Lib.CoreTypes

sidDefaultMetaclass :: NSID
sidDefaultMetaclass = coreId "DefaultMetaclass"

rtDefaultMetaclass :: TypeDescriptor
rtDefaultMetaclass = ResolvedType
                       sidDefaultMetaclass
                       sidDefaultMetaclass
                       (ClassDefinition [])

getKindDefaultMetaclass :: MStat m s => Scope s -> m s (ValueRef s)
getKindDefaultMetaclass s =
    catchError (snd <$> scopeLookupRef s sidDefaultMetaclass errorInitializer)
               (\ _ -> do
                  metaclass <- liftToST buildKindDefaultMetaclass
                  scopeAddItemRef s
                                  sidDefaultMetaclass
                                  rtDefaultMetaclass
                                  metaclass
                  return metaclass
               )
    where
      buildKindDefaultMetaclass =
          fixST $ \ self -> do
            members <- newArray (0, 0) KindUnit
            newSTRef $ KindObject self members
      errorInitializer _ _ = throwError $ InternalError
          "kind::DefaultMetaclass should not be initialized during scope lookup"

