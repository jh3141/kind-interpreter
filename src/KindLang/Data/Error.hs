{-# LANGUAGE FlexibleContexts #-}
module KindLang.Data.Error where

import KindLang.Data.BasicTypes
import KindLang.Data.AST
import KindLang.Util.Control
import Control.Monad.Except

type Reason = String

data KindError =
    InternalError Reason |
    InvalidImport NSID Reason |
    IdentifierNotFound NSID |
    NotNamespace NSID NSID |
    IsNamespace NSID |
    TypeError NSID Reason |
    InvalidApplication [TypeDescriptor] [TypeDescriptor] |
    TypeMismatch TypeDescriptor TypeDescriptor String |
    TypeKindError TypeDescriptor String |
    AccessViolation NSID Visibility |
    NoAppropriateInstance NSID TypeDescriptor
    deriving (Show, Eq)

errorWhen :: MonadError KindError m => Bool -> KindError -> m ()
errorWhen True e = throwError e
errorWhen False _ = return ()

errorWhenNot :: MonadError KindError m => Bool -> KindError -> m ()
errorWhenNot b = errorWhen (not b)

errorIfNothing :: MonadError KindError m => Maybe a -> KindError -> m a
errorIfNothing Nothing e = throwError e
errorIfNothing (Just r) _ = return r

replaceErrorIdentifier :: NSID -> KindError -> KindError
replaceErrorIdentifier e (InvalidImport _ r) = InvalidImport e r
replaceErrorIdentifier e (IdentifierNotFound _) = IdentifierNotFound e
replaceErrorIdentifier e (NotNamespace _ i) = NotNamespace e i
replaceErrorIdentifier e (TypeError _ r) = TypeError e r
replaceErrorIdentifier e (AccessViolation _ v) = AccessViolation e v
replaceErrorIdentifier _ e = e
