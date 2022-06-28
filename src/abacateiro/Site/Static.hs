{- | Ema app to serve static files without any dynamic generation involved.
 Usually you want to combine this with your real site.
-}
module Site.Static (StaticRoute, Model, Options (..)) where

import Ema
import Prefix
import Site.Static.Options
import Ema.Route.Encoder
import System.FilePath.Posix ((</>))
import Routes (strPrism, StringRoute, FileRoute' (..))

newtype Route = Route {unStaticRoute :: FilePath}
  deriving stock (Show, Eq)
  deriving (StringRoute) via (FileRoute' String)

instance IsRoute Route where
  type RouteModel Route = Options
  routeEncoder = prismRouteEncoder strPrism
  allRoutes _ = [Route ""]

instance EmaSite Route where
  type SiteArg Route = (Options, ()) -- Sorry
  siteInput _ (opt,_) = pure $ pure opt
  siteOutput _ m (Route fp) = Ema.AssetStatic $ mount m </> fp

type StaticRoute = PrefixedRoute Route

type Model = Options
