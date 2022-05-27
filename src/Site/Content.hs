-- |

module Site.Content (ContentRoute, Options (..)) where
import Place
import Ema hiding (PrefixedRoute)
import Org.Types
import Ema.Route.Encoder
import Relude.Extra (keys)
import Org.Exporters.Heist (Exporter, documentSplices)
import Heist.Interpreted
import Data.Generics.Product.Fields
import Data.Map ((!), member, delete, insert)
import Optics.Core
import System.FilePath (stripExtension, (</>), dropExtension)
import System.UnionMount (FileAction (..))
import System.UnionMount qualified as UM
import Render (HState, heistOutput, HeistRoute, renderAsset)
import System.FilePattern
import Org.Parser
import LaTeX
import JSON
import Heist (HeistState)

data Options = Options
  { mount :: FilePath
  , exclude :: [FilePattern]
  , serveAt :: FilePath
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Options where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Options where
  parseJSON = genericParseJSON customOptions

data Model = Model
  { docs :: Map FilePath OrgDocument
  , serveAt :: FilePath
  , hState :: HState
  }
  deriving (Generic)

newtype Route = Route FilePath
  deriving (Eq, Show)
  deriving newtype (IsString, ToString)

instance IsRoute Route where
  type RouteModel Route = Model
  routeEncoder = mkRouteEncoder \m ->
    prism' (<> ".html") (\fp -> do
        fp' <- stripExtension ".html" fp
        guard (fp' `member` docs m) $> Route fp'
      )
    % iso id toString
  allRoutes m = Route <$> keys (docs m)

instance EmaSite Route where
  type SiteArg Route = Options
  siteInput _ _ opt = Dynamic <$>
    UM.mount source include exclude' (Model mempty mempty Nothing)
      (const handler)
    where
      source = mount opt
      include = [((), "**/*.org")]
      exclude' = exclude opt
      handler fp =
        let place = Place fp source
            key = dropExtension fp
        in \case
          Refresh _ () -> do
            orgdoc <- parseOrgIO defaultOrgOptions (source </> fp)
            preamble <- preambilizeKaTeX place orgdoc
            let orgdoc' = mapContent (first (preamble :)) orgdoc
            pure (over (field @"docs") (insert key orgdoc'))
          Delete -> pure (over (field @"docs") (delete key))
  siteOutput = heistOutput renderDoc

renderDoc :: Route -> RouteEncoder Model Route -> Model -> HeistState Exporter -> Asset LByteString
renderDoc (Route fp) _ m = renderAsset $
  callTemplate "ContentPage" $ documentSplices (docs m ! fp)

type ContentRoute = HeistRoute (PrefixedRoute Route)
