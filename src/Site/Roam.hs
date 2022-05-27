-- |

module Site.Roam (RoamRoute) where
import Ema hiding (PrefixedRoute)
import Place
import Site.Roam.Model qualified as M
import Site.Roam.Process
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import Org.Parser (parseOrgIO, defaultOrgOptions)
import Site.Roam.Render (renderIndex, renderPost, renderGraph)
import System.FilePath ((</>))
import LaTeX (preambilizeKaTeX)
import Render (heistOutput, HeistRoute)

instance EmaSite M.Route where
  type SiteArg M.Route = M.Options
  siteInput _ _ src = Dynamic <$>
      UM.mount source include exclude M.model0
        (const handler)
    where
      source = M.mount src
      include = [((), "**/*.org")]
      exclude = M.exclude src
      handler fp = \case
        Refresh _ () -> do
          orgdoc <- parseOrgIO defaultOrgOptions (source </> fp)
          let place = Place fp source
          preamble <- preambilizeKaTeX place orgdoc
          pure $ processRoam place preamble orgdoc
        Delete -> pure $ M.deleteRD fp
  siteOutput = heistOutput \case
    M.Route_Index -> renderIndex
    M.Route_Graph -> const renderGraph
    M.Route_Post uid -> renderPost uid

type RoamRoute = HeistRoute (PrefixedRoute M.Route)
