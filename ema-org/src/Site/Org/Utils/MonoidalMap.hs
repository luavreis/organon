{-# LANGUAGE DeriveTraversable #-}

module Site.Org.Utils.MonoidalMap where

import GHC.Exts (IsList (..))
import Data.Map qualified as M

newtype MonoidalMap k a = MonoidalMap (Map k a)
  deriving (Eq, Ord, Read, Show, Typeable, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

instance (Ord k, Semigroup a) => IsList (MonoidalMap k a) where
  type Item (MonoidalMap k a) = (k, a)
  fromList x = MonoidalMap $ M.fromListWith (<>) x
  toList (MonoidalMap x) = GHC.Exts.toList x

instance (Ord k, Semigroup a) => Semigroup (MonoidalMap k a) where
  MonoidalMap a <> MonoidalMap b = MonoidalMap $ M.unionWith (<>) a b

instance (Ord k, Semigroup a) => Monoid (MonoidalMap k a) where
  mempty = MonoidalMap mempty
  mconcat a = MonoidalMap $ M.unionsWith (<>) (coerce a :: [Map k a])
