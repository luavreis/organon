{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- |

module Prefix (PrefixedRoute') where
import Ema ( Dynamic(Dynamic), IsRoute(..), EmaSite(..) )
import qualified Data.Text as T
import Ema.Route.Encoder ( decodeRoute, encodeRoute, mapRouteEncoderRoute, mkRouteEncoder )
import Optics.Core ( prism' )
import Data.Generics.Wrapped ( _Wrapped, _Unwrapped )
import Data.Generics.Product ( Subtype(upcast, smash) )
import System.FilePath (addTrailingPathSeparator, (</>))

newtype PrefixedRoute' a = PrefixedRoute' {unPrefixRoute :: a}
  deriving (Generic)
  deriving newtype (Eq, Show)

data PrefixOptions = PrefixOptions { mount :: FilePath, serveAt :: FilePath }
  deriving (Generic)

type HasPrefix s = Subtype PrefixOptions s

instance (IsRoute a, HasPrefix (RouteModel a)) => IsRoute (PrefixedRoute' a) where
  type RouteModel (PrefixedRoute' a) = RouteModel a
  routeEncoder =
    mkRouteEncoder \m@(serveAt . upcast -> pfx) ->
     Optics.Core.prism' ((pfx </>) . encodeRoute newEnc m)
            (decodeRoute newEnc m <=< stripPrefix pfx)
    where
      newEnc = mapRouteEncoderRoute _Wrapped (routeEncoder @a)
      stripPrefix "" = Just
      stripPrefix p =
        fmap toString . T.stripPrefix (toText $ addTrailingPathSeparator p) . toText
  allRoutes = map PrefixedRoute' . allRoutes

instance (EmaSite a, (x,y) ~ SiteArg a, HasPrefix (RouteModel a), HasPrefix x) => EmaSite (PrefixedRoute' a) where
  type SiteArg (PrefixedRoute' a) = SiteArg a
  siteInput act enc arg = do
    Dynamic (m0, h) <- (siteInput @a) act newEnc arg
    pure $ Dynamic (setf m0, h . (. setf))
    where
      setf = smash (upcast @PrefixOptions (fst arg))
      newEnc = mapRouteEncoderRoute _Unwrapped enc
  siteOutput enc m r = siteOutput newEnc m (unPrefixRoute r)
    where
      newEnc = mapRouteEncoderRoute _Unwrapped enc
