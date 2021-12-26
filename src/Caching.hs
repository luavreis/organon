{-# LANGUAGE BangPatterns #-}
-- |

module Caching where
import Data.Binary (Binary, encode, decode)
import Control.Monad.Logger
import Control.Monad.IO.Unlift
import Control.Concurrent.STM.TVar (modifyTVar)
import UnliftIO (MonadUnliftIO)
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import System.UnionMount
import System.FilePattern (FilePattern)
import Data.Binary.Instances.UnorderedContainers
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Writer
import UnliftIO.Directory (doesFileExist)

type Cache = (HashMap LByteString LByteString, Set LByteString)

type CacheT model m = WriterT (Endo model) (StateT Cache m)

getCache :: forall m k. Monad m => CacheT k m Cache
getCache = get

lookupCache :: (MonadIO m, Binary b, Binary a) => b -> CacheT k m (Maybe a)
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
  :: forall a b k m. (Binary a, Binary b, MonadLogger m, MonadIO m)
  => (a -> m b) -> a -> CacheT k m b
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
  -> (Change source tag -> CacheT model m ())
  -> LVar model
  -> m (Maybe Cmd)
cachedUnionMountOnLVar sources pats ignore model0 handleAction modelLVar = do
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
    updateModel <-
      interceptExceptions id $
      runReaderT (handleAction changes) cacheVar
    atomically (takeTMVar initialized) >>= \case
      False -> do
        LVar.set modelLVar $ updateModel model0
      True -> do
        LVar.modify modelLVar updateModel

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
  -> (b -> FilePath -> FileAction () -> CacheT model m ())
  -> LVar model
  -> m (Maybe Cmd)
cachedMountOnLVar folder pats ignore var0 toAction' =
  let tag0 = ()
      sources = one (tag0, folder)
  in cachedUnionMountOnLVar sources pats ignore var0 $ \ch -> do
    let fsSet = (fmap . fmap . fmap . fmap) void $ fmap Map.toList <$> Map.toList ch
    (\(tag, xs) -> uncurry (toAction' tag) `chainM` xs) `chainM` fsSet
  where
    chainM :: Monad n => (x -> n (a -> a)) -> [x] -> n (a -> a)
    chainM f = fmap chain . mapM f
      where
        chain ::  [a -> a] -> a -> a
        chain = flip (foldr ($))
