-- |

module Site.Roam (RoamRoute) where
import Ema hiding (PrefixedRoute)
import Place
import Site.Roam.Model
import Site.Roam.Options qualified as O
import Site.Roam.Process
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import Site.Roam.Render (renderIndex, renderPost, renderGraph, renderAttachment)
import Render (heistOutput, HeistRoute)

instance EmaSite Route where
  type SiteArg Route = O.Options
  siteInput _ _ src = Dynamic <$>
      UM.mount source include exclude model0
        (const handler)
    where
      source = O.mount src
      include = [((), "**/*.org")]
      exclude = O.exclude src
      handler fp = \case
        Refresh _ () -> appEndo <$> runReaderT (processRoam fp) src
        Delete -> pure $ deleteRD fp
  siteOutput = heistOutput \case
    Route_Index -> renderIndex
    Route_Graph -> const renderGraph
    Route_Post uid -> renderPost uid
    Route_Attach path -> const (renderAttachment path)

type RoamRoute = HeistRoute (PrefixedRoute Route)
