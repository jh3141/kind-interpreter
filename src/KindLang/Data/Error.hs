module KindLang.Data.Error where

import KindLang.Data.BasicTypes

type Reason = String

data KindError =
    InternalError Reason |
    InvalidImport ScopedID Reason |
    IdentifierNotFound ScopedID |
    NotNamespace ScopedID ScopedID |
    TypeError ScopedID Reason
    deriving (Show, Ord, Eq)

type KErr a = Either KindError a
             
-- nb KErr is a monad because (Either a) is a monad. no need to define an instance
-- here.

