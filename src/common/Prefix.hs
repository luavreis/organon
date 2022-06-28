{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- |

module Prefix (PrefixedRoute) where
import Ema ( Dynamic(Dynamic), IsRoute(..), EmaSite(..) )
import qualified Data.Text as T
import Ema.Route.Encoder (mapRouteEncoderRoute, mkRouteEncoder, applyRouteEncoder )
import Optics.Core
import Data.Generics.Wrapped ( _Wrapped, _Unwrapped )
import Data.Generics.Product ( Subtype(upcast, smash) )
import System.FilePath (addTrailingPathSeparator, (</>))

newtype PrefixedRoute a = PrefixedRoute {unPrefixRoute :: a}
  deriving (Generic)
  deriving newtype (Eq, Show)

data PrefixOptions = PrefixOptions { mount :: FilePath, serveAt :: FilePath }
  deriving (Generic)

type HasPrefix s = Subtype PrefixOptions s

instance (IsRoute a, HasPrefix (RouteModel a)) => IsRoute (PrefixedRoute a) where
  type RouteModel (PrefixedRoute a) = RouteModel a
  routeEncoder =
    mkRouteEncoder \m@(serveAt . upcast -> pfx) ->
     let rp = applyRouteEncoder newEnc m
     in prism' (pfx </>) (stripPrefix pfx) % rp
    where
      newEnc = mapRouteEncoderRoute _Wrapped (routeEncoder @a)
      stripPrefix "" s = Just s
      stripPrefix p s | p == s = Just ""
      stripPrefix p s =
        toString <$> T.stripPrefix (toText $ addTrailingPathSeparator p) (toText s)
  allRoutes = map PrefixedRoute . allRoutes

instance (EmaSite a, (x,y) ~ SiteArg a, HasPrefix (RouteModel a), HasPrefix x) => EmaSite (PrefixedRoute a) where
  type SiteArg (PrefixedRoute a) = SiteArg a
  siteInput act arg = do
    Dynamic (m0, h) <- (siteInput @a) act arg
    pure $ Dynamic (setf m0, h . (. setf))
    where
      setf = smash (upcast @PrefixOptions (fst arg))
  siteOutput rp m r = siteOutput (rp % _Unwrapped) m (unPrefixRoute r)
