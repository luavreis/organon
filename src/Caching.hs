{-# LANGUAGE ConstraintKinds #-}
-- |

module Caching where
import Data.Binary
import Control.Monad.Logger
import Control.Monad.IO.Unlift
import Control.Concurrent.STM.TVar (modifyTVar)
import UnliftIO (MonadUnliftIO)
import Data.LVar as LVar
import System.UnionMount
import System.FilePattern (FilePattern)
import Data.Binary.Instances.UnorderedContainers
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import UnliftIO.Directory (doesFileExist)

type Cache = (HashMap LByteString LByteString, Set LByteString)

newtype CacheT m a = CacheT { runCacheT :: TVar Cache -> m a }

getCache :: Applicative m => CacheT m (TVar Cache)
getCache = CacheT pure

lookupCache :: (MonadIO m, Binary b, Binary a) => b -> CacheT m (Maybe a)
lookupCache k = do
  let kbs = encode k
  cVar <- getCache
  atomically $ do
    (hmap, set) <- readTVar cVar
    modifyTVar cVar (mapSnd (Set.insert kbs))
    return $ fmap decode (HMap.lookup kbs hmap)
  where
    mapSnd f (x, y) = (x, f y)

fromCacheOrCompute
  :: forall a b m. (Binary a, Binary b, MonadLogger m, MonadIO m)
  => (a -> m b) -> a -> CacheT m b
fromCacheOrCompute f x = do
  my <- lookupCache x
  case my of
    Just (y :: b) -> pure y
    Nothing -> do
      lift $ logInfoNS "fromCache" "Computing values."
      y <- lift (f x)
      cVar <- getCache
      atomically $
        modifyTVar cVar $ mapFst (HMap.insert (encode x) (encode y))
      return y
  where
    mapFst f (x, y) = (f x, y)

instance MonadTrans CacheT where
  lift = CacheT . const

instance Functor m => Functor (CacheT m) where
  fmap f c = CacheT $ fmap f . runCacheT c

instance Monad m => Applicative (CacheT m) where
    pure = CacheT . const . pure
    cf <*> ca = CacheT $ \s -> do
      a <- runCacheT ca s
      f <- runCacheT cf s
      pure $ f a

instance Monad m => Monad (CacheT m) where
  c >>= k = CacheT $ \s -> do
    a <- runCacheT c s
    runCacheT (k a) s

instance MonadIO m => MonadIO (CacheT m) where
  liftIO = CacheT . const . liftIO

instance MonadUnliftIO m => MonadUnliftIO (CacheT m) where
  withRunInIO inner =
    CacheT $ \s ->
    withRunInIO $ \run ->
    inner (run . flip runCacheT s)


cachedUnionMountOnLVar
  :: ( MonadIO m
     , MonadUnliftIO m
     , MonadLogger m
     , Binary model
     , Ord source
     , Ord tag
     )
  => Set (source, FilePath)
  -> [(tag, FilePattern)]
  -> [FilePattern]
  -> model
  -> (Change source tag -> model -> CacheT m model)
  -> LVar model
  -> m (Maybe Cmd)
cachedUnionMountOnLVar sources pats ignore model0 handleAction modelLVar = do
  let initialModel = model0
  initialized <- newTMVarIO False

  cacheFileExists <- doesFileExist "website.cache"
  cacheVar <- newTVarIO =<<
    if cacheFileExists
    then do
      decoded <- liftIO $ decodeFileOrFail "website.cache"
      case decoded of
        Left (_, e) -> do
          logErrorNS "Caching" "Failed to decode website.cache"
          pure mempty
        Right (c :: Cache) -> pure c
    else do
      logInfoNS "Caching" "website.cache does not exist. A new one will be created"
      pure mempty

  unionMount sources pats ignore $ \changes -> do
    let updateModel = handleAction changes
    atomically (takeTMVar initialized) >>= \case
      False -> do
        LVar.set modelLVar =<< runCacheT (updateModel initialModel) cacheVar
      True -> do
        LVar.set modelLVar =<< runCacheT (updateModel =<< LVar.get modelLVar) cacheVar

    cache <- atomically $ do
      modifyTVar cacheVar $ \(x, set) ->
        (HMap.filterWithKey (const . flip Set.member set) x, Set.empty)
      readTVar cacheVar
    when (not $ null (snd cache)) $
      liftIO $ encodeFile "website.cache" cache

    atomically $ putTMVar initialized True
    pure Nothing

-- | Simplified variation of `cachedUnionMountOnLVar` with exactly one source.
cachedMountOnLVar
  :: forall model m b.
     ( MonadIO m
     , Binary model
     , MonadUnliftIO m
     , MonadLogger m
     , Show b
     , Ord b
     )
  => FilePath
  -> [(b, FilePattern)]
  -> [FilePattern]
  -> model
  -> (b -> FilePath -> FileAction () -> model -> CacheT m model)
  -> LVar model
  -> m (Maybe Cmd)
cachedMountOnLVar folder pats ignore var0 toAction' =
  let tag0 = ()
      sources = one (tag0, folder)
  in cachedUnionMountOnLVar sources pats ignore var0 $ \ch -> do
    let fsSet = (fmap . fmap . fmap . fmap) void $ fmap Map.toList <$> Map.toList ch
    (\(tag, xs) -> uncurry (toAction' tag) `chainM` xs) `chainM` fsSet
  where
    chainM :: Monad n => (x -> a -> n a) -> [x] -> a -> n a
    chainM f xs =
      composeM $ map f xs
      where
        composeM :: Monad n => [a -> n a] -> a -> n a
        composeM = foldr (<=<) pure
