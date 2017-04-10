{-# LANGUAGE RankNTypes, FlexibleInstances, MultiParamTypeClasses  #-}
module KindLang.Data.KStat where

import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.STRef
import KindLang.Data.BasicTypes
import KindLang.Data.Error
import KindLang.Data.AST
import KindLang.Util.Control
import KindLang.Data.MStat

-- FIXME look into changing maps-stored-in-ST into actual mutable structures
data KStatRoot s = KStatRoot
    {
      kstatLoadedModules :: STRef s (Map.Map NSID (STRef s Module)),
      kstatDefinitions :: STRef s (Map.Map NSID Definition)
    }

newtype KStat s a =
    KStat { runKStat :: ReaderT (KStatRoot s) (ExceptT KindError (ST s)) a }

instance MStat KStat s where
    liftToST = KStat . lift . lift
    kstatNewRef = liftToST . newSTRef
    kstatReadRef = liftToST . readSTRef
    kstatWriteRef r v = liftToST $ writeSTRef r v

instance Functor (KStat s) where
    fmap f = KStat . fmap f . runKStat

instance Applicative (KStat s) where
    pure = KStat . pure
    f <*> a = KStat $ (runKStat f) <*> (runKStat a)

instance Monad (KStat s) where
    return = KStat . return
    m >>= f = KStat ((runKStat m) >>= (runKStat . f))

instance MonadError KindError (KStat s) where
    throwError = KStat . throwError
    catchError m f = KStat (catchError (runKStat m) (runKStat . f))

instance MonadReader (KStatRoot s) (KStat s) where
    ask = KStat ask

initKStat :: KStat s a -> ST s (Either KindError a)
initKStat r = do
    loadedModules <- newSTRef Map.empty
    definitions <- newSTRef Map.empty
    runExceptT $ runReaderT (runKStat r)
                            (KStatRoot loadedModules definitions)

runToIO :: (forall s . KStat s a) -> IO a
runToIO r = either (error . show) return $ runST $ initKStat r

runToEither :: (forall s . KStat s a) -> Either KindError a
runToEither r = runST $ initKStat r

expectNoErrors :: String -> (forall s . KStat s a) -> a
expectNoErrors err r = case runToEither r of
                         Left err' -> error $ err ++ ": " ++ show err'
                         Right v   -> v

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

nop :: Monad m => a -> m ()
nop _ = return ()

