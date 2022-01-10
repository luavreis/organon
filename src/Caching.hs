{-# LANGUAGE BangPatterns #-}
-- |

module Caching where
import Data.LVar (LVar)
import Control.Monad.Logger
import Ema.Helper.FileSystem
import Control.Monad.IO.Unlift
import qualified Data.Set as Set
import Control.Monad.Trans.Writer.Strict
import qualified Data.LVar as LVar
import UnliftIO.Directory (doesFileExist)
import qualified Data.HashMap.Strict as HMap
import System.FilePattern (FilePattern, (?==))
import Data.Binary.Instances.UnorderedContainers ()
import UnliftIO (readTBQueue, newTBQueueIO, race, TBQueue, BufferMode (BlockBuffering, LineBuffering), hSetBuffering, hFlush, finally)
import Data.Binary (Binary, encode, decode, decodeFileOrFail, encodeFile)

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
     , MonadLogger m
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
  => (a -> m (Maybe b)) -> a -> m (Maybe b)
fromCacheOrCompute f x = do
  lookupCache x >>= \case
    Just (y :: b) -> return $ Just y
    Nothing -> do
      logInfoNS "fromCache" "Computing value."
      f x >>= \case
        Nothing -> return Nothing
        Just y -> do
          modify (\ ~(cache, bookkeeper) -> (HMap.insert (encode x) (encode y) cache, bookkeeper))
          return $ Just y

cachedMountOnLVar
  :: forall m model source tag.
     ( MonadUnliftIO m
     , MonadLogger m
     , Binary model
     , Binary source
     , Ord source
     , Ord tag
     )
  => Set (source, FilePath)
  -> [(tag, FilePattern)]
  -> [FilePattern]
  -> model
  -> (tag -> FilePath -> source -> FileAction () -> CacheT model m ())
  -> LVar model
  -> m ()
cachedMountOnLVar sources pats ignore model0 handleAction modelLVar = do
  initialized <- newTMVarIO False

  cacheFileExists <- doesFileExist "website.cache"

  cacheVar <- newMVar =<<
    if cacheFileExists
    then (liftIO $ decodeFileOrFail "website.cache")
        >>= \case
          Left (_, e) -> do
            logErrorNS "Caching" $ "Failed to decode website.cache:\n" <> (toText e)
            pure mempty
          Right (c :: (CacheMap, Set (LByteString, FilePath))) -> pure c
    else do
      logInfoNS "Caching" "website.cache does not exist. A new one will be created"
      pure mempty

  let loop = disjointUnionMount sources pats ignore $ \changes -> do

        previousCache <- takeMVar cacheVar

        (updateModel, unfilteredCache) <-
          runCacheT (mapM (\ ~(x,y,z,w) -> handleAction x y z w) changes)
                    (fst previousCache, mempty)
          & interceptExc

        let updatedFilepaths = map (\ ~(_,y,_,_) -> y) changes
            (hmap, usedKeys) = unfilteredCache
            newBookeeper = snd previousCache
                          & Set.filter (\ ~(k, fp) ->
                                            fp `notElem` updatedFilepaths
                                            || k `Set.member` usedKeys)
                          & Set.union (Set.cartesianProduct usedKeys (fromList updatedFilepaths))
            filteredCache = HMap.filterWithKey (\ ~ k _ -> any (\ ~(bk, _) -> k == bk) newBookeeper) hmap
            newCache = (filteredCache, newBookeeper)

        atomically (takeTMVar initialized) >>= \case
          False -> do
            LVar.set modelLVar $ updateModel model0
          True -> do
            LVar.modify modelLVar updateModel

        putMVar cacheVar newCache

        atomically $ putTMVar initialized True
        pure Nothing

  mcmd <- finally loop $ do
    logInfoN "Saving cache..."
    liftIO $ encodeFile "website.cache" =<< readMVar cacheVar

  whenJust mcmd $ \Cmd_Remount -> do
    logInfoNS "Mounting" "!! Remount suggested !!"
    LVar.set modelLVar model0 -- Reset the model
    cachedMountOnLVar sources pats ignore model0 handleAction modelLVar

  where
    interceptExc mt = do
      ~(f, c) <- mt
      f' <- interceptExceptions id (pure f)
      return (f', c)

disjointUnionMount
  :: forall source tag m.
     ( MonadIO m
     , MonadUnliftIO m
     , MonadLogger m
     , Ord source
     , Ord tag
     )
  => Set (source, FilePath)
  -> [(tag, FilePattern)]
  -> [FilePattern]
  -> ([(tag, FilePath, source, FileAction ())] -> m (Maybe Cmd))
  -> m (Maybe Cmd)
disjointUnionMount sources pats ignore handleAction = do
  fmap (join . rightToMaybe) $ do
    -- Initial traversal of sources
    changes0 :: [(tag, FilePath, source, FileAction ())] <-
      fmap snd . runWriterT $ do
        forM_ sources $ \(src, folder) -> do
          taggedFiles <- filesMatchingWithTag folder pats ignore
          forM_ taggedFiles $ \(tag, fs) -> do
            forM_ fs $ \fp -> do
              tell [(tag, fp, src, Refresh Existing ())]
    void . withBlockBuffering $ handleAction changes0
    -- Run fsnotify on sources
    q :: TBQueue (x, FilePath, Either (FolderAction ()) (FileAction ())) <- liftIO $ newTBQueueIO 1
    race (onChange q (toList sources)) $ do
      let loop = do
            (src, fp, actE) <- atomically $ readTBQueue q
            let shouldIgnore = any (?== fp) ignore
            case actE of
              Left _ -> do
                if shouldIgnore
                  then do
                    log LevelWarn "Unhandled folder event on an ignored path"
                    loop
                  else do
                    -- We don't know yet how to deal with folder events. Just reboot the mount.
                    log LevelWarn "Unhandled folder event; suggesting a re-mount"
                    pure $ Just Cmd_Remount
              Right act -> do
                case guard (not shouldIgnore) >> getTag pats fp of
                  Nothing -> loop
                  Just tag -> do
                    mcmd <- withBlockBuffering $ handleAction [(tag, fp, src, act)]
                    maybe loop (pure . pure) mcmd
      loop

  where
    withBlockBuffering f =
      hSetBuffering stdout (BlockBuffering Nothing)
        *> f
        <* (hSetBuffering stdout LineBuffering >> hFlush stdout)
