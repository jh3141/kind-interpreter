{-# LANGUAGE RankNTypes #-}
module KindLang.Data.KStat where

import Control.Monad.Except
import Control.Monad.ST.Trans
import KindLang.Data.Error
import KindLang.Util.Control

type KStat s a = STT s (Except KindError) a

runToIO :: (forall s . KStat s a) -> IO a
runToIO r = either (error . show) return $ runExcept $ runST r

runToEither :: (forall s . KStat s a) -> Either KindError a
runToEither r = runExcept $ runST r

expectNoErrors :: String -> (forall s . KStat s a) -> a
expectNoErrors err r = rightOrFail err $ runExcept $ runST r
