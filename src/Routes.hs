{-# LANGUAGE UndecidableInstances #-}
-- | General instances to be used with deriving via.
module Routes where
import Ema
import Ema.Route.Encoder
import Data.Map (keys, keysSet)
import Data.Set (member)
import Optics.Core
import System.FilePath (stripExtension, (<.>))

class StringRoute a where
  stringfy :: (a -> String, String -> Maybe a)

instance StringRoute FilePath where
  stringfy = (id, Just)

instance StringRoute Text where
  stringfy = ((<.> "html") . toString, fmap fromString . stripExtension "html")

newtype SetRoute a = SetRoute a
  deriving (Eq, Show, Generic)
  deriving newtype (IsString, ToString)

instance (StringRoute a, Ord a, Show a) => IsRoute (SetRoute a) where
  type RouteModel (SetRoute a) = Set a
  routeEncoder = mkRouteEncoder \m ->
    prism' (\(SetRoute x) -> fst stringfy x)
           (\fp -> do
               x <- snd stringfy fp
               guard (x `member` m) $> SetRoute x)
  allRoutes m = SetRoute <$> toList m

newtype MapRoute a b = MapRoute a
  deriving (Eq, Show, Generic)
  deriving newtype (IsString, ToString)

instance (StringRoute a, Ord a, Show a) => IsRoute (MapRoute a b) where
  type RouteModel (MapRoute a b) = Map a b
  routeEncoder = mapRouteEncoder
                 equality
                 coercedTo
                 keysSet
                 (routeEncoder @(SetRoute a))
  allRoutes m = MapRoute <$> keys m
