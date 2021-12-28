{-# LANGUAGE BangPatterns #-}
-- |

module Caching where
import Data.Binary (Binary, encode, decode, decodeFileOrFail, encodeFile)
import Control.Monad.Logger
import Control.Monad.IO.Unlift
import Data.LVar (LVar)
import qualified Data.LVar as LVar
import System.UnionMount
import System.FilePattern (FilePattern)
import Data.Binary.Instances.UnorderedContainers ()
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.Writer
import UnliftIO.Directory (doesFileExist)

type CacheMap = HashMap LByteString LByteString

type Cache = (CacheMap, Set LByteString)

type CacheT model m = WriterT (Endo model) (StateT Cache m)

runCacheT :: Monad m => CacheT model m a -> Cache -> m (model -> model, Cache)
runCacheT mc c = mapFst appEndo <$> runStateT (execWriterT mc) c
  where
    mapFst f ~(x, y) = (f x, y)

lookupCache
  :: forall a b m.
     ( Binary a
     , Binary b
     , MonadState Cache m
     )
  => a -> m (Maybe b)
lookupCache k = do
  let kbs = encode k
  ~(hmap, _) <- get
  modify (\ ~(x,y) -> (x, Set.insert kbs y))
  return $ fmap decode (HMap.lookup kbs hmap)

fromCacheOrCompute
  :: forall a b m.
     ( Binary a
     , Binary b
     , MonadLogger m
     , MonadState Cache m
     )
  => (a -> m b) -> a -> m b
fromCacheOrCompute f x = do
  lookupCache x >>= \case
    Just (y :: b) -> do
      pure y
    Nothing -> do
      logInfoNS "fromCache" "Computing values."
      y <- f x
      modify (\ ~(c, set) -> (HMap.insert (encode x) (encode y) c, set))
      return y

cachedUnionMountOnLVar
  :: forall m model source tag.
     ( MonadIO m
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
    then (liftIO $ decodeFileOrFail "website.cache")
        >>= \case
          Left (_, e) -> do
            logErrorNS "Caching" $ "Failed to decode website.cache:\n" <> (toText e)
            pure mempty
          Right (c :: CacheMap) -> pure c
    else do
      logInfoNS "Caching" "website.cache does not exist. A new one will be created"
      pure mempty

  unionMount sources pats ignore $ \changes -> do

    previousCache <- readTVarIO cacheVar

    (updateModel, unfilteredCache) <-
      runCacheT (handleAction changes) (previousCache, mempty)
      & interceptExc

    let filteredCache = filterCache unfilteredCache

    atomically (takeTMVar initialized) >>= \case
      False -> do
        LVar.set modelLVar $ updateModel model0
      True -> do
        LVar.modify modelLVar updateModel

    atomically $ writeTVar cacheVar filteredCache

    when (not $ null (snd unfilteredCache)) $
      liftIO $ encodeFile "website.cache" filteredCache

    atomically $ putTMVar initialized True
    pure Nothing
    where
      filterCache :: Cache -> CacheMap
      filterCache (hmap, set) = HMap.filterWithKey (const . flip Set.member set) hmap

      interceptExc :: m (a -> a, b) -> m (a -> a, b)
      interceptExc mt = do
        ~(f, c) <- mt
        f' <- interceptExceptions id (pure f)
        return (f', c)

-- | Simplified variation of `cachedUnionMountOnLVar` with exactly one source.
cachedMountOnLVar
  :: forall model m b.
     ( MonadIO m
     , Binary model
     , MonadUnliftIO m
     , MonadLogger m
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
    chainM :: forall a x n. Monad n => (x -> WriterT (Endo a) n ()) -> [x] -> WriterT (Endo a) n ()
    chainM f x = foldr (>>) (pure ()) $ map f x
