{- | Ema app to serve static files without any dynamic generation involved.
 Usually you want to combine this with your real site.
-}
module Site.Static (StaticRoute, Model, Options (..)) where

import Ema hiding (PrefixedRoute)
import Place
import Site.Static.Options
import Ema.Route.Encoder
import System.FilePath.Posix ((</>))

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

type Model = Options
