{-# OPTIONS_GHC -Wno-orphans #-}

module Site.Org (Route, Model (..)) where

import Control.Monad.Logger (MonadLogger, logDebugN)
import Data.IxSet.Typed qualified as Ix
import Data.Map ((!))
import Data.Map qualified as Map
import Ema
import Org.Exporters.Processing
import Org.Parser (defaultOrgOptions, parseOrgIO)
import Site.Org.Cache (Cache)
import Site.Org.Graph (renderGraph)
import Site.Org.LaTeX (processKaTeX, processLaTeX)
import Site.Org.Model
import Site.Org.Options qualified as O
import Site.Org.Process
import Site.Org.Render (OndimOutput (OAsset), renderPost)
import System.FilePath (takeDirectory, (</>))
import System.FilePattern (FilePattern)
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import UnliftIO (MonadUnliftIO, pooledForConcurrently)
import UnliftIO.Exception (evaluate)

data FileType = OrgFile | OtherFile
  deriving (Eq, Ord, Show)

instance EmaSite Route where
  type SiteArg Route = (O.Options, TVar Cache)
  type SiteOutput Route = OndimOutput
  siteInput _ (opt, cache) = do
    pages' :: Dynamic m Pages <-
      mconcat <<$>> sequenceA <$> forM sources \(O.Source srcKey srcFp) ->
        Dynamic <$> mountConcurrently srcFp include exclude mempty (handler srcKey srcFp)
    pure $ Model <$> pages' ?? sourcesMap
    where
      sources = O.mount opt
      sourcesMap = fromList $ map O.srcToPair sources
      include = (OrgFile, "**/*.org") : zip (repeat OtherFile) (O.staticPatterns opt)
      exclude = O.exclude opt
      handler srcKey srcFp OrgFile file = \case
        Refresh _ () -> do
          logDebugN $ "Loading " <> prettyOrgPath orgPath

          let initialResolution x = runPipeline do
                gatherKeywords x
                pure $ pruneDoc x

          doc <-
            parseOrgIO defaultOrgOptions absfp
              <&> initialResolution
              >>= processLaTeX (O.latexOptions opt) cache absfp
              >>= processKaTeX absfp
          -- >>= processFileLinks (srcKey, srcFp) relDir (O.orgAttachDir opt)
          newPages <- evaluate . force =<< loadOrgFile opt sourcesMap orgPath mempty doc
          pure $ Ix.insertList newPages . deleteAll
        Delete -> pure deleteAll
        where
          orgPath = fromRawPath srcKey file
          deleteAll m =
            let matching = m Ix.@= orgPath
             in foldr Ix.delete m matching
          absfp = srcFp </> file
          relDir = takeDirectory file
      handler _srcKey _srcFp OtherFile _file = const do
        pure id
  siteOutput rp m =
    pure . \case
      Route_Graph -> OAsset $ renderGraph rp m
      Route_Static (coerce -> OrgPath s fp) ->
        OAsset $ const $ pure $ AssetStatic (_mSources m ! s </> fp)
      Route_Page identifier -> renderPost identifier rp m

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
