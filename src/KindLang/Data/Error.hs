module KindLang.Data.Error where

import KindLang.Data.BasicTypes
import KindLang.Data.AST
import Control.Monad.Except
import KindLang.Util.Control
    
type Reason = String

data KindError =
    InternalError Reason |
    InvalidImport NSID Reason |
    IdentifierNotFound NSID |
    NotNamespace NSID NSID |
    TypeError NSID Reason |
    InvalidApplication [TypeDescriptor] [TypeDescriptor] |
    TypeMismatch TypeDescriptor TypeDescriptor String |
    TypeKindError TypeDescriptor String |
    AccessViolation NSID Visibility |
    NoAppropriateInstance NSID TypeDescriptor
    deriving (Show, Eq)

type KErr a = Except KindError a
             
-- nb KErr is a monad because (Either a) is a monad. no need to define an instance
-- here.

errorWhen :: Bool -> KindError -> KErr ()
errorWhen True e = throwError e
errorWhen False _ = return ()

errorWhenNot :: Bool -> KindError -> KErr ()
errorWhenNot b = errorWhen (not b)

errorIfNothing :: Maybe a -> KindError -> KErr a
errorIfNothing Nothing e = throwError e
errorIfNothing (Just r) _ = return r
                            

replaceErrorIdentifier :: NSID -> KindError -> KindError
replaceErrorIdentifier e (InvalidImport _ r) = InvalidImport e r
replaceErrorIdentifier e (IdentifierNotFound _) = IdentifierNotFound e
replaceErrorIdentifier e (NotNamespace _ i) = NotNamespace e i
replaceErrorIdentifier e (TypeError _ r) = TypeError e r
replaceErrorIdentifier e (AccessViolation _ v) = AccessViolation e v
replaceErrorIdentifier _ e = e
                             
expectNoErrors :: String -> KErr a -> a
expectNoErrors err = rightOrFail err . runExcept 
