{-# LANGUAGE MultiParamTypeClasses,FlexibleContexts #-}
module KindLang.Data.MStat where

import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.Reader
import Data.STRef
import KindLang.Data.Error

class (Monad (m s), MonadError KindError (m s)) =>
      MStat m s where
    liftToST :: ST s a -> m s a
    kstatNewRef :: a -> m s (STRef s a)
    kstatReadRef :: STRef s a -> m s a
    kstatWriteRef :: STRef s a -> a -> m s ()
