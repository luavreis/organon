-- |

module Site.Roam (RoamRoute, Model (..)) where
import Ema
import Site.Roam.Options qualified as O
import Site.Roam.Model
import Site.Roam.Process
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import Site.Roam.Render (renderIndex, renderPost, renderGraph)
import Render (heistOutput)
import Site.Roam.Route
import Prefix (PrefixedRoute')
import OrgAttach (renderAttachment, processAttachInDoc)
import Control.Monad.Trans.Writer
import Place
import Org.Parser (parseOrgIO, defaultOrgOptions)

instance EmaSite Route where
  type SiteArg Route = O.Options
  siteInput _ _ opt = Dynamic <$>
      UM.mount source include exclude model0
        (const handler)
    where
      source = O.mount opt
      include = [((), "**/*.org")]
      exclude = O.exclude opt
      run x = appEndo <$> runReaderT (execWriterT x) opt
      handler fp action =
        (case action of
          Refresh _ () -> run $ do
            doc' <- parseOrgIO defaultOrgOptions (absolute place)
            tell =<< processAttachInDoc doc'
            tell =<< lift (processRoam doc' place)
          Delete -> pure id)
        <&> (. deleteAllFromFile fp)
        where
          place = Place fp source
  siteOutput = heistOutput \case
    RouteIndex' -> renderIndex
    RouteGraph' -> const renderGraph
    RoutePost' uid -> renderPost uid
    RouteAttach path -> const (renderAttachment path)
    _ -> error "dammit ghc"

type RoamRoute = PrefixedRoute' Route
