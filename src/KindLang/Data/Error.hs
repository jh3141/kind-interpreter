module KindLang.Data.Error where

import KindLang.Data.AST

type Reason = String

data KindError =
    InternalError Reason |
    InvalidImport ScopedID Reason
    deriving (Show, Ord, Eq)
