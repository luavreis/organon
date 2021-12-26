-- | 

module PandocTransforms.Utilities
  ( module Text.Pandoc
  , module Text.Pandoc.Walk
  , setMeta
  , pooledWalk
  , stripFilePrefix
  ) where
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc.Walk (Walkable, walk, walkM)
import System.FilePath
import Data.List (stripPrefix)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (pooledMapConcurrentlyN)


-- | Concurrent walkM

pooledWalk
  :: (MonadUnliftIO m, Walkable a b, Traversable t)
  => Int
  -> (a -> m a)
  -> t b -> m (t b)
pooledWalk n f = pooledMapConcurrentlyN n (walkM f)


-- | Filepath utilities

stripFilePrefix :: FilePath -> FilePath -> Maybe FilePath
stripFilePrefix f g = stripPrefix (addTrailingPathSeparator $ normalise f) (normalise g)
