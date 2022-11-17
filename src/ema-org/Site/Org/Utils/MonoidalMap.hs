-- (I can only hope this derivation actually works)
{-# LANGUAGE DeriveTraversable #-}

module Site.Org.Utils.MonoidalMap where

import Data.Map qualified as M

newtype MonoidalMap k a = MonoidalMap (Map k a)
  deriving (Eq, Ord, Show, Typeable, Generic, Functor, Foldable, Traversable)

instance (Ord k, Semigroup a) => Semigroup (MonoidalMap k a) where
  MonoidalMap a <> MonoidalMap b = MonoidalMap $ M.unionWith (<>) a b

instance (Ord k, Semigroup a) => Monoid (MonoidalMap k a) where
  mempty = MonoidalMap mempty
  mconcat a = MonoidalMap $ M.unionsWith (<>) (coerce a :: [Map k a])
