{-# LANGUAGE UndecidableInstances #-}
-- | General instances to be used with deriving via.
module Routes where
import Ema
import Data.Map (keys, keysSet)
import Data.Set (member)
import Optics.Core
import System.FilePath (stripExtension, (<.>))

class StringRoute a where
  strPrism' :: (a -> String, String -> Maybe a)

strPrism :: StringRoute s => Prism' String s
strPrism = uncurry prism' strPrism'

newtype FileRoute' a = FileRoute' a
  deriving (Eq, Ord, Show)
  deriving newtype (IsString, ToString)

instance (IsString s, ToString s) => StringRoute (FileRoute' s) where
  strPrism' = (toString, Just . fromString)

newtype HtmlRoute s = HtmlRoute s
  deriving (Eq, Ord, Show)
  deriving newtype (IsString, ToString)

instance (IsString s, ToString s) => StringRoute (HtmlRoute s) where
  strPrism' = ((<.> "html") . toString, fmap fromString . stripExtension "html")

newtype SetRoute a = SetRoute a
  deriving (Eq, Show, Generic)
  deriving newtype (IsString, ToString)

instance (StringRoute a, Ord a, Show a) => IsRoute (SetRoute a) where
  type RouteModel (SetRoute a) = Set a
  routePrism m = toPrism_ $
    prism' (\(SetRoute x) -> fst strPrism' x)
           (\fp -> do
               x <- snd strPrism' fp
               guard (x `member` m) $> SetRoute x)
  routeUniverse m = SetRoute <$> toList m

newtype MapRoute a b = MapRoute a
  deriving (Eq, Show, Generic)
  deriving newtype (IsString, ToString)

instance (StringRoute a, Ord a, Show a) => IsRoute (MapRoute a b) where
  type RouteModel (MapRoute a b) = Map a b
  routePrism (keysSet -> m) =
    let p = fromPrism_ $ routePrism @(SetRoute a) m
    in toPrism_ $ p % coercedTo
  routeUniverse m = MapRoute <$> keys m
