module KindLang.Data.Error where

import KindLang.Data.BasicTypes

type Reason = String

data KindError =
    InternalError Reason |
    InvalidImport ScopedID Reason |
    IdentifierNotFound ScopedID |
    NotNamespace ScopedID ScopedID
    deriving (Show, Ord, Eq)

type KErr a = Either KindError a
             
