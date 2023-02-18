{-# OPTIONS_GHC -Wno-orphans #-}

module Site.Org (Route, Model (..)) where

import Control.Monad.Logger (MonadLogger, logDebugN)
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as Map
import Ema
import Org.Parser (parseOrgIO)
import Site.Org.Graph (renderGraph)
import Site.Org.Model
import Site.Org.Options qualified as O
import Site.Org.Process
import Site.Org.Render (OndimOutput (AssetOutput), renderPost)
import System.FilePath ((</>))
import System.FilePattern (FilePattern)
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import UnliftIO (MonadUnliftIO, pooledForConcurrently)
import UnliftIO.Exception (evaluate)

data FileType = OrgFile | OtherFile
  deriving (Eq, Ord, Show)

instance EmaSite Route where
  type SiteArg Route = O.Options
  type SiteOutput Route = OndimOutput
  siteInput _ opt = do
    pages' :: Dynamic m Pages <-
      mconcat <<$>> sequenceA <$> forM sources \source ->
        Dynamic <$> mountConcurrently (O.dir source) include exclude mempty (handler source)
    pure $ Model <$> pages' ?? opt
    where
      sources = O.mount opt
      include = (OrgFile, "**/*.org") : zip (repeat OtherFile) (O.staticPatterns opt)
      exclude = O.exclude opt
      handler source OrgFile file = \case
        Refresh _ () -> do
          logDebugN $ "Loading " <> prettyOrgPath orgPath

          newPages <-
            parseOrgIO (O.parserSettings opt) absfp
              >>= loadOrgFile opt orgPath
              >>= evaluate . force

          pure $ Ix.insertList newPages . deleteAll
        Delete -> pure deleteAll
        where
          orgPath = fromRawPath source file
          deleteAll m =
            let matching = m Ix.@= orgPath
             in foldr Ix.delete m matching
          absfp = O.dir source </> file
      handler _source OtherFile _file = const do
        -- TODO: perhaps refresh timestamps
        pure id
  siteOutput rp m =
    pure . \case
      Route_Graph ->
        renderGraph rp m
      Route_Static ix ->
        let OrgPath s fp = coerce ix
         in AssetOutput $ pure $ AssetStatic (O.dir s </> fp)
      Route_Page identifier ->
        renderPost identifier rp m

-- | Conturrent version of `UnionMount.mount`
mountConcurrently ::
  forall model m b.
  ( MonadIO m,
    MonadUnliftIO m,
    MonadLogger m,
    Show b,
    Ord b
  ) =>
  FilePath ->
  [(b, FilePattern)] ->
  [FilePattern] ->
  model ->
  (b -> FilePath -> FileAction () -> m (model -> model)) ->
  m (model, (model -> m ()) -> m ())
mountConcurrently folder pats ignore var0 toAction =
  UM.unionMount (one ((), folder)) pats ignore var0 $ \chg ->
    chain . join <$> forM (Map.toList chg) \(tag, chg') ->
      pooledForConcurrently (Map.toList chg') $
        uncurry (toAction tag) . second void
  where
    chain :: [a -> a] -> a -> a
    chain = flip $ foldl' (&)
