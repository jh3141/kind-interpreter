module KindLang.Data.Error where

import KindLang.Data.BasicTypes
import KindLang.Data.AST
    
type Reason = String

data KindError =
    InternalError Reason |
    InvalidImport NSID Reason |
    IdentifierNotFound NSID |
    NotNamespace NSID NSID |
    TypeError NSID Reason |
    InvalidApplication [TypeDescriptor] [TypeDescriptor] |
    TypeMismatch TypeDescriptor String |
    AccessViolation NSID Visibility
    deriving (Show, Eq)

type KErr a = Either KindError a
             
-- nb KErr is a monad because (Either a) is a monad. no need to define an instance
-- here.

