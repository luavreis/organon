-- |

module PandocTransforms.Utilities
  ( module Text.Pandoc
  , module Text.Pandoc.Walk
  , MReaderT
  , setMeta
  , pooledWalk
  , stripFilePrefix
  ) where
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Builder (setMeta)
import System.FilePath
import Data.List (stripPrefix)
import UnliftIO.Async (pooledMapConcurrentlyN)
import Control.Monad.IO.Unlift

-- | Concurrent walkM

type MReaderT s m = ReaderT (MVar s) m

instance {-# OVERLAPPING #-} MonadIO m => MonadState s (MReaderT s m) where
  get = do
    sVar <- ask
    readMVar sVar
  put s = do
    sVar <- ask
    void $ swapMVar sVar s
  state f = do
    sVar <- ask
    os <- takeMVar sVar
    let (y, ns) = f os
    putMVar sVar ns
    pure y

pooledWalk
  :: forall a b l m s t.
    ( Walkable a b
    , MonadTrans l
    , MonadUnliftIO m
    , Monad m
    , MonadIO (l m)
    , MonadState s (l m)
    , Traversable t
    , Show s
    )
  => Int
  -> (a -> MReaderT s m a)
  -> t b -> l m (t b)
pooledWalk n f x = do
  var <- newMVar =<< get
  let computation = pooledMapConcurrentlyN n (walkM f) x
  result <- lift $ runReaderT computation var
  put =<< readMVar var
  pure result

-- | Filepath utilities

stripFilePrefix :: FilePath -> FilePath -> Maybe FilePath
stripFilePrefix f g = stripPrefix (addTrailingPathSeparator $ normalise f) (normalise g)
