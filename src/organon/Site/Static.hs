{- | Ema app to serve static files without any dynamic generation involved.
 Usually you want to combine this with your real site.
-}
module Site.Static (StaticRoute, Model, Options (..)) where

import Ema
import System.FilePath.Posix ((</>))
import Routes (strPrism, StringRoute, FileRoute' (..))
import Config (Options, mount)
import Render (OndimOutput (OAsset))

newtype Route = Route {unStaticRoute :: FilePath}
  deriving stock (Show, Eq)
  deriving (StringRoute) via (FileRoute' String)

instance IsRoute Route where
  type RouteModel Route = Options
  routePrism _ = toPrism_ strPrism
  routeUniverse _ = [Route ""]

instance EmaSite Route where
  type SiteArg Route = (Options, ()) -- Sorry
  type SiteOutput Route = OndimOutput
  siteInput _ (opt,_) = pure $ pure opt
  siteOutput _ m (Route fp) =
    pure $ OAsset $ const $ Ema.AssetStatic $ mount m </> fp

type StaticRoute = Route

type Model = Options
