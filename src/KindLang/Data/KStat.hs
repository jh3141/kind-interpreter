{-# LANGUAGE RankNTypes #-}
module KindLang.Data.KStat where

import Control.Monad.Except
import Control.Monad.ST.Trans
import Control.Monad.Reader
import qualified Data.Map as Map
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.AST
import KindLang.Util.Control

-- FIXME look into changing maps-stored-in-ST into actual mutable structures
data KStatRoot s = KStatRoot
    {
      loadedModules :: STRef s (Map.Map NSID (STRef s Module)),
      definitions :: STRef s (Map.Map NSID Definition)
    }
type KStat s a = ReaderT (KStatRoot s) (STT s (Except KindError)) a

initKStat :: Monad m => ReaderT (KStatRoot s) (STT s m) a -> STT s m a
initKStat r = do
    loadedModules <- newSTRef Map.empty
    definitions <- newSTRef Map.empty
    runReaderT r (KStatRoot loadedModules definitions)

runToIO :: (forall s . KStat s a) -> IO a
runToIO r = either (error . show) return $ runExcept $ runST $ initKStat r

runToEither :: (forall s . KStat s a) -> Either KindError a
runToEither r = runExcept $ runST $ initKStat r

expectNoErrors :: String -> (forall s . KStat s a) -> a
expectNoErrors err r = rightOrFail err $ runExcept $ runST $ initKStat r
