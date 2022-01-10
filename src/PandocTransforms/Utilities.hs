-- |

module PandocTransforms.Utilities
  ( module Text.Pandoc
  , module Text.Pandoc.Walk
  , module Text.Pandoc.Shared
  , module PandocTransforms.Utilities
  , setMeta
  ) where
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.Shared
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

-- | Reader / writer

readerOptions :: ReaderOptions
readerOptions = def {
  readerExtensions =
      extensionsFromList
      [ Ext_citations
      , Ext_smart
      , Ext_fancy_lists
      ]
  }

writerOptions :: WriterOptions
writerOptions =
  def
  { writerHTMLMathMethod = MathJax ""
  -- , writerExtensions =
  --     extensionsFromList
  --     [ Ext_citations
  --     ]
  }

writeHtml :: PandocMonad m => Pandoc -> m Text
writeHtml = writeHtml5String writerOptions

-- | AST utilities

getMeta :: Pandoc -> Meta
getMeta (Pandoc meta _) = meta

setMetaP :: Text -> Text -> Pandoc -> Pandoc
setMetaP k l (Pandoc meta blocks) =
  Pandoc (setMeta k l meta) blocks
