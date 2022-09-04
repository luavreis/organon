{-# OPTIONS_GHC -Wno-orphans #-}
-- |

module Site.Roam (RoamRoute, Model (..)) where
import Ema
import Site.Roam.Options qualified as O
import Site.Roam.Model
import Site.Roam.Process
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import Site.Roam.Render (renderIndex, renderPost, renderGraph)
import OrgAttach (renderAttachment, processAttachInDoc)
import Place
import Org.Parser (parseOrgIO, defaultOrgOptions)
import Cache (Cache)
import LaTeX (processLaTeX)
import Render (OndimOutput (OAsset))

instance EmaSite Route where
  type SiteArg Route = (O.Options, TVar Cache)
  type SiteOutput Route = OndimOutput
  siteInput _ arg@(opt,_) = Dynamic <$>
      UM.mount source include exclude model0
        (const handler)
    where
      source = O.mount opt
      include = [((), "**/*.org")]
      exclude = O.exclude opt
      run x = runReaderT x arg
      handler fp action =
        (case action of
          Refresh _ () ->
            run do
              doc' <- parseOrgIO defaultOrgOptions (absolute place)
                      >>= processLaTeX place
              att <- processAttachInDoc doc'
              roam <- processRoam doc' place
              pure $ att . roam
          Delete -> pure id)
        <&> (. deleteAllFromFile fp)
        where
          place = Place fp source
  siteOutput rp m = pure . \case
    Route_Index -> renderIndex rp m
    Route_Graph -> OAsset $ renderGraph m
    Route_Post uid -> renderPost uid rp m
    Route_Attach path -> OAsset $ const $ pure $ renderAttachment path m

type RoamRoute = Route
