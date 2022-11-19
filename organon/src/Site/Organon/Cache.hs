module Site.Organon.Cache
  ( Cache (..),
    cache0,
    loadCache,
  )
where

import Control.Monad.Logger (MonadLogger, logDebugN, logWarnN)
import Data.Binary (Binary, decodeFileOrFail)
import Data.Binary.Instances.UnorderedContainers ()
import Data.HashMap.Strict qualified as HMap
import UnliftIO (IOException, MonadUnliftIO, catch)

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
