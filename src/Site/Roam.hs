-- |

module Site.Roam (RoamRoute, Model (..)) where
import Ema hiding (PrefixedRoute)
import Place
import Site.Roam.Options qualified as O
import Site.Roam.Model
import Site.Roam.Process
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import Site.Roam.Render (renderIndex, renderPost, renderGraph, renderAttachment)
import Render (heistOutput)
import Site.Roam.Route

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
    RouteIndex' -> renderIndex
    RouteGraph' -> const renderGraph
    RoutePost' uid -> renderPost uid
    RouteAttach' path -> const (renderAttachment path)
    _ -> error "dammit ghc"

type RoamRoute = PrefixedRoute Route
