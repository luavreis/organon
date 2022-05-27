{- | Ema app to serve static files without any dynamic generation involved.
 Usually you want to combine this with your real site.
-}
module Site.Static (StaticRoute, Options (..)) where

import Ema hiding (PrefixedRoute)
import Place
import Ema.Route.Encoder
import JSON
import System.FilePath.Posix ((</>))

data Options = Options
  { mount :: FilePath
  , serveAt :: FilePath
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Options where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Options where
  parseJSON = genericParseJSON customOptions

newtype Route = Route {unStaticRoute :: FilePath}
  deriving stock (Show, Eq)
  deriving newtype (IsString, ToString)

instance IsRoute Route where
  type RouteModel Route = Options
  routeEncoder = stringRouteEncoder
  allRoutes m = [Route $ mount m]

instance EmaSite Route where
  type SiteArg Route = Options
  siteInput _ _ opt = pure $ pure opt
  siteOutput _ m (Route fp) = Ema.AssetStatic $ mount m </> fp

type StaticRoute = PrefixedRoute Route
