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
      kstatLoadedModules :: STRef s (Map.Map NSID (STRef s Module)),
      kstatDefinitions :: STRef s (Map.Map NSID Definition)
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

kstatNewRef :: a -> KStat s (STRef s a)
kstatNewRef = lift . newSTRef

kstatReadRef :: STRef s a -> KStat s a
kstatReadRef = lift . readSTRef

kstatWriteRef :: STRef s a -> a -> KStat s ()
kstatWriteRef r v = lift $ writeSTRef r v

kstatStoreModule :: Module -> KStat s ()
kstatStoreModule m =
    case moduleName m of
      Nothing -> do
        -- no ID implies module is the default module, so shouldn't ever
        -- be referred to from externally, therefore it doesn't need to be
        -- stored.
        return ()

      Just sid -> do
        modulesRef <- kstatLoadedModules <$> ask
        modules <- kstatReadRef modulesRef
        moduleRef <- kstatNewRef m
        kstatWriteRef modulesRef $ Map.insert sid moduleRef modules

kstatFindModule :: NSID -> KStat s (Maybe Module)
kstatFindModule sid = do
    modulesRef <- kstatLoadedModules <$> ask
    loadedModules <- kstatReadRef modulesRef
    case Map.lookup sid loadedModules of
      Nothing -> return Nothing
      Just mRef -> Just <$> kstatReadRef mRef

