{-# LANGUAGE UndecidableInstances #-}
-- |

module Site.Content (ContentRoute, Model (..), Options (..)) where
import Place
import Ema
import Org.Types
import Ema.Route.Encoder
import Org.Exporters.Heist (Exporter, documentSplices)
import Heist.Interpreted
import Data.Generics.Product.Fields
import Data.Map ((!), delete, insert)
import Optics.Core
import System.FilePath ((</>), dropExtension, takeDirectory)
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import Render (HeistS, heistOutput, renderAsset)
import Org.Parser
import LaTeX hiding (preamble)
import Heist (HeistState)
import Site.Content.Options
import qualified Generics.SOP as SOP
import Ema.Route.GenericClass
import Routes
import Site.Content.Links
import UnliftIO.Concurrent (threadDelay)

data Model = Model
  { mount :: FilePath
  , serveAt :: FilePath
  , docs :: Map Text OrgDocument
  , files :: Set FilePath
  , heistS :: HeistS
  }
  deriving (Generic)

type DocRoute = MapRoute Text OrgDocument

type FileRoute = SetRoute FilePath

data ModelI = MI {docs :: Map Text OrgDocument, files :: Set FilePath}
  deriving (Generic, SOP.Generic)

data Route
  = Doc DocRoute
  | File FileRoute
  deriving (Eq, Show, Generic, SOP.Generic)
  deriving (IsRoute) via (GenericRoute ModelI Model Route)

instance EmaSite Route where
  type SiteArg Route = Options
  siteInput _ _ opt = Dynamic <$>
    UM.mount source include exclude' (Model "" mempty mempty mempty Nothing)
      (const handler)
    where
      source = mount (opt :: Options)
      include = [((), "**/*.org")]
      exclude' = exclude opt
      handler fp =
        let place = Place fp source
            key = toText $ dropExtension fp
        in \case
          Refresh _ () -> do
            threadDelay 100
            orgdoc <- parseOrgIO defaultOrgOptions (source </> fp)
            preamble <- preambilizeKaTeX place orgdoc
            let orgdoc' = mapContent (first (preamble :)) orgdoc
                (orgdoc'', files') = processLinks (takeDirectory fp) orgdoc'
            orgdoc''' <- runReaderT (processLaTeX place orgdoc'') opt
            pure (over (field @"docs") (insert key orgdoc''') . over (field @"files") (files' <>))
          Delete -> pure (over (field @"docs") (delete key))
  siteOutput = heistOutput renderDoc

renderDoc :: Route -> RouteEncoder Model Route -> Model -> HeistState Exporter -> Asset LByteString
renderDoc (Doc (MapRoute fp)) _ m = renderAsset $
  callTemplate "ContentPage" $ documentSplices (docs (m :: Model) ! fp)
renderDoc (File (SetRoute fp)) _ m = const $ AssetStatic (mount (m :: Model) </> fp)

type ContentRoute = PrefixedRoute' Route
