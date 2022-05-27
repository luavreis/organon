{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
-- |

module Place where
import System.FilePath
import Ema hiding (PrefixedRoute)
import qualified Data.Text as T
import Ema.Route.Encoder
import Optics.Core
import Data.Generics.Wrapped (_Wrapped)
import Data.Generics.Product

data Place = Place {relative :: FilePath, dir :: FilePath}

absolute :: Place -> FilePath
absolute p = dir p </> relative p

newtype PrefixedRoute a = PrefixedRoute {unPrefixRoute :: a}
  deriving (Generic)
  deriving newtype (Eq, Show)

prefixIso :: Iso' a (PrefixedRoute a)
prefixIso = _Wrapped

type HasPrefix s = HasField' "serveAt" s FilePath

instance (IsRoute a, HasPrefix (RouteModel a)) => IsRoute (PrefixedRoute a) where
  type RouteModel (PrefixedRoute a) = RouteModel a
  routeEncoder =
    mkRouteEncoder \m@(view (field' @"serveAt") -> pfx) ->
     prism' ((pfx </>) . encodeRoute newEnc m)
            (decodeRoute newEnc m <=< stripPrefix pfx)
    where
      newEnc = mapRouteEncoderRoute prefixIso (routeEncoder @a)
      stripPrefix "" = Just
      stripPrefix p =
        fmap toString . T.stripPrefix (toText $ addTrailingPathSeparator p) . toText
  allRoutes = map PrefixedRoute . allRoutes

instance (IsRoute a, EmaSite a, HasPrefix (RouteModel a), HasPrefix (SiteArg a)) => EmaSite (PrefixedRoute a) where
  type SiteArg (PrefixedRoute a) = SiteArg a
  siteInput act enc arg@(view (field' @"serveAt") -> pfx) = do
    Dynamic (m0, h) <- (siteInput @a) act newEnc arg
    pure $ Dynamic (setf m0, h . (. setf))
    where
      setf = set (field' @"serveAt") pfx
      newEnc = mapRouteEncoderRoute (re prefixIso) enc
  siteOutput enc m r = siteOutput newEnc m (unPrefixRoute r)
    where
      newEnc = mapRouteEncoderRoute (re prefixIso) enc
