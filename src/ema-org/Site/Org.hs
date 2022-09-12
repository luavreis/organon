{-# OPTIONS_GHC -Wno-orphans #-}

module Site.Org (Route, Model (..)) where

import Data.IxSet.Typed qualified as Ix
import Ema
import Org.Parser (defaultOrgOptions, parseOrgIO)
import Site.Org.Cache (Cache)
import Site.Org.Graph (renderGraph)
import Site.Org.LaTeX (processKaTeX, processLaTeX)
import Site.Org.Model
import Site.Org.Options qualified as O
import Site.Org.Process
import Site.Org.Render (OndimOutput (OAsset), renderPost)
import System.FilePath ((</>), takeDirectory)
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import Data.Map ((!))
import Site.Org.OrgAttach (processFileLinks)

data FileType = OrgFile | OtherFile
  deriving (Eq, Ord, Show)

instance EmaSite Route where
  type SiteArg Route = (O.Options, TVar Cache)
  type SiteOutput Route = OndimOutput
  siteInput _ (opt, cache) = do
    pages' :: Dynamic m Pages <-
      mconcat <<$>> sequenceA
        <$> forM sources \(O.Source srcKey srcFp) ->
          Dynamic
            <$> UM.mount srcFp include exclude mempty (handler srcKey srcFp)
    pure $ Model <$> pages' ?? sourcesMap
    where
      sources = O.mount opt
      sourcesMap = fromList $ map O.srcToPair sources
      include = [(OrgFile, "**/*.org")] ++ zip (repeat OtherFile) (O.staticPatterns opt)
      exclude = O.exclude opt
      handler srcKey srcFp OrgFile file = \case
        Refresh _ () -> do
          (doc, staticFiles) <-
            parseOrgIO defaultOrgOptions absfp
              >>= processLaTeX (O.latexOptions opt) cache absfp
              >>= processKaTeX absfp
              >>= processFileLinks (srcKey, srcFp) relDir (O.orgAttachDir opt)
          putStrLn $ show staticFiles
          newPages <- loadOrgFile opt sourcesMap orgPath staticFiles doc
          pure $ Ix.insertList newPages . deleteAll
        Delete -> pure $ deleteAll
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
      Route_Graph -> OAsset $ renderGraph m
      Route_Static (coerce -> OrgPath s fp) ->
        OAsset $ const $ pure $ AssetStatic (_mSources m ! s </> fp)
      Route_Page identifier -> renderPost identifier rp m
