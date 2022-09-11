module Site.Org.Cache where

import Control.Monad.Logger (MonadLogger, logDebugN, logWarnN)
import Data.Binary (Binary, decodeFileOrFail)
import Site.Org.LaTeX.Types (LaTeXCache)
import UnliftIO (IOException, MonadUnliftIO, catch)

newtype Cache = Cache
  { latexCache :: LaTeXCache
  }
  deriving (Generic)
  deriving anyclass (Binary)

cache0 :: Cache
cache0 = Cache mempty

loadCache :: (MonadUnliftIO m, MonadLogger m) => FilePath -> TVar Cache -> m ()
loadCache cFile cVar =
  ( liftIO (decodeFileOrFail cFile) >>= \case
      Right x -> atomically $ writeTVar cVar x
      Left (_, s) -> do
        logWarnN "Error decoding cache file. Starting with an empty one."
        logDebugN $ toText s
  )
    `catch` \(e :: IOException) -> do
      logWarnN $ show e
