{-# OPTIONS_GHC -Wno-orphans #-}

module Site.Org (Route, Model (..)) where

import Control.Monad.Logger (MonadLogger, logDebugN)
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as Map
import Ema (Asset (AssetStatic), Dynamic (..), EmaSite (..))
import Org.Parser (parseOrgDocIO)
import Site.Org.Model (
  Model (..),
  OrgPath (OrgPath),
  Pages,
  StaticFile (StaticFile),
  prettyOrgPath,
 )
import Site.Org.Options (
  Options (exclude, mount, parserSettings, staticPatterns),
  Source (dir),
 )
import Site.Org.Process (loadOrgFile)
import Site.Org.Render (renderPost, Ondim)
import Site.Org.Route (Route (..))
import System.FilePath ((</>))
import System.FilePattern (FilePattern)
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import UnliftIO (MonadUnliftIO, pooledForConcurrently)
import UnliftIO.Exception (evaluate)

data FileType = OrgFile | OtherFile
  deriving (Eq, Ord, Show)

instance EmaSite Route where
  type SiteArg Route = Options
  type SiteOutput Route = Ondim (Asset LByteString)
  siteInput _ opt = do
    pages' :: Dynamic m Pages <-
      mconcat <<$>> sequenceA <$> forM sources \source ->
        Dynamic <$> mountConcurrently source.dir include exclude mempty (handler source)
    pure $ Model <$> pages' ?? opt
    where
      sources = opt.mount
      include = (OrgFile, "**/*.org") : map (OtherFile,) opt.staticPatterns
      exclude = opt.exclude
      handler source OrgFile file = \case
        Refresh _ () -> do
          logDebugN $ "Loading " <> prettyOrgPath orgPath

          newPages <-
            parseOrgDocIO opt.parserSettings absfp
              >>= loadOrgFile opt source orgPath
              >>= evaluate . force

          pure $ Ix.insertList newPages . deleteAll
        Delete -> pure deleteAll
        where
          orgPath = OrgPath source file
          deleteAll m =
            let matching = m Ix.@= orgPath
             in foldr Ix.delete m matching
          absfp = source.dir </> file
      handler _source OtherFile _file = const do
        -- TODO: perhaps refresh timestamps
        pure id
  siteOutput rp m =
    pure . \case
      Route_Static ix ->
        let OrgPath s fp = coerce ix
         in pure $ AssetStatic (s.dir </> fp)
      Route_Page identifier ->
        renderPost identifier rp m

-- | Conturrent version of `UnionMount.mount`
mountConcurrently ::
  forall model m b.
  ( MonadIO m
  , MonadUnliftIO m
  , MonadLogger m
  , Show b
  , Ord b
  ) =>
  FilePath ->
  [(b, FilePattern)] ->
  [FilePattern] ->
  model ->
  (b -> FilePath -> FileAction () -> m (model -> model)) ->
  m (model, (model -> m ()) -> m ())
mountConcurrently folder pats ignore var0 toAction =
  UM.unionMount (one ((), folder)) pats ignore var0 \chg ->
    chain . join <$> forM (Map.toList chg) \(tag, chg') ->
      pooledForConcurrently (Map.toList chg') $
        uncurry (toAction tag) . second void
  where
    chain :: [a -> a] -> a -> a
    chain = flip $ foldl' (&)
