{-# LANGUAGE MultiParamTypeClasses,FlexibleContexts #-}
module KindLang.Data.MStat where

import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.Reader
import Data.STRef
import KindLang.Data.Error

-- | MStat is a typeclass implemented by all monads capable of running Kind interpreter processes.
-- It is used to allow (1) abstraction between the IO and ST monads to use for mutable data storage, and
-- (2) to minimize the dependencies required in order to declare functions that use it.

class (Monad (m s), MonadError KindError (m s)) =>
      MStat m s where

    -- | Perform a mutable data operation
    liftToST :: ST s a -> m s a
    -- | create a new mutable variable
    kstatNewRef :: a -> m s (STRef s a)
    -- | read a mutable variable
    kstatReadRef :: STRef s a -> m s a
    -- | write a mutable variable
    kstatWriteRef :: STRef s a -> a -> m s ()
    -- | generate a new unique identifier
    kstatUniqueId :: m s Integer
    -- | cause an internal error message to be generated
    internalError :: String -> m s a
    internalError = throwError . InternalError
