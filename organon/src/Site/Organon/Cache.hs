module Site.Organon.Cache (
  Cache (..),
  cacheDynamic,
)
where

import Control.Monad.Logger (MonadLogger, logDebugN, logInfoNS, logWarnN)
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import Data.Binary.Instances.UnorderedContainers ()
import Data.HashMap.Strict qualified as HMap
import Ema.Dynamic (Dynamic (..))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (IOException, catch, finally)

newtype Cache = Cache
  { cacheStore :: HMap.HashMap LByteString LByteString
  }
  deriving (Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (Binary)

cache0 :: Cache
cache0 = Cache mempty

loadCache :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (TVar Cache)
loadCache cFile = do
  cVar <- newTVarIO cache0
  (liftIO (decodeFileOrFail cFile) >>= handle cVar)
    `catch` \(e :: IOException) -> do
      logWarnN "Error reading cache file. Starting with an empty one."
      logWarnN $ show e
  return cVar
  where
    handle cVar cache =
      case cache of
        Right x -> atomically $ writeTVar cVar x
        Left (_, s) -> do
          logWarnN "Error decoding cache file. Starting with an empty one."
          logDebugN $ toText s

cacheDynamic :: (MonadUnliftIO m, MonadLogger m) => FilePath -> m (Dynamic m (TVar Cache))
cacheDynamic file = do
  cVar <- loadCache file
  let close _ =
        forever (threadDelay maxBound) `finally` do
          logInfoNS "cacheDynamic" "Writing cache to disk."
          cache <- readTVarIO cVar
          liftIO $ encodeFile file cache
  pure $ Dynamic (cVar, close)
