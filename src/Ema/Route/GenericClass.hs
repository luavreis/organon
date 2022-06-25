{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- Used only for subRouteEncoder
{-# LANGUAGE EmptyCase #-}
-- |

module Ema.Route.GenericClass where
import Prelude
import Ema
import Generics.SOP as SOP
import Optics.Core hiding (to)
import Ema.Route.Encoder
import GHC.TypeLits
import Data.Generics.Wrapped
import Data.Generics.Product.Subtype
import Data.Generics.Product.Fields
import Data.Generics.Sum.Constructors

newtype GenericRoute (ref :: Type) (m :: Type) r = GenericRoute { unGenericRoute :: r }
  deriving (Prelude.Generic)

type GenericRoute' m r = GenericRoute m m r

type family FilterNonEmpty (xs :: [Type]) :: [Type] where
  FilterNonEmpty '[] = '[]
  FilterNonEmpty (() : xs) = FilterNonEmpty xs
  FilterNonEmpty (x : xs) = x : FilterNonEmpty xs

class NPNonEmpty (xs :: [Type]) where
  npNonEmptyIso :: Iso' (NP I xs) (NP I (FilterNonEmpty xs))

instance NPNonEmpty '[] where
  npNonEmptyIso = equality

instance (NPNonEmpty xs) => NPNonEmpty (() : xs) where
  npNonEmptyIso = iso
                  (\(I () :* xs) -> view npNonEmptyIso xs)
                  (\xs -> I () :* review npNonEmptyIso xs)

instance {-# OVERLAPPABLE #-}
  ( NPNonEmpty xs
  , (x : FilterNonEmpty xs) ~ FilterNonEmpty (x : xs)
  ) =>
  NPNonEmpty (x : xs)
  where
  npNonEmptyIso = iso from' to'
    where
      from' :: NP I (x : xs) -> NP I (x : FilterNonEmpty xs)
      from' (x :* xs) = x :* view npNonEmptyIso xs
      to' :: NP I (x : FilterNonEmpty xs) -> NP I (x : xs)
      to' (x :* xs) = x :* review npNonEmptyIso xs

type family Heads (xs :: [[Type]]) :: [Type] where
  Heads '[] = '[]
  Heads ('[a] : xs) = a : Heads xs
  Heads (x : xs) = TypeError ('Text "Each constructor should have a single route field")

class AllSingleton (xs :: [[Type]]) where
  allSingletonIso :: Iso' (SOP I xs) (NS I (Heads xs))

instance AllSingleton '[] where
  allSingletonIso = iso (\case {}) (\case {})

instance (AllSingleton xs) => AllSingleton ('[a] ': xs) where
  allSingletonIso = iso from' to'
    where
      from' :: SOP I ('[a] : xs) -> NS I (a : Heads xs)
      from' (SOP (Z (I x :* Nil))) = Z (I x)
      from' (SOP (S xs)) = S $ view allSingletonIso (SOP xs)
      to' :: NS I (a : Heads xs) -> SOP I ('[a] : xs)
      to' (Z (I x)) = SOP (Z (I x :* Nil))
      to' (S xs) = let (SOP ys) = review allSingletonIso xs
                   in SOP (S ys)

type IsASumType a = (SOP.Generic a, AllSingleton (Code a))

type ASumCode a = Heads (Code a)

isASumTypeIso :: IsASumType a => Iso' a (NS I (ASumCode a))
isASumTypeIso = iso from to % allSingletonIso

newtype NSumRoute xs = NSumRoute { toNS :: NS I xs }
  deriving (Prelude.Generic)

instance IsRoute (NSumRoute '[]) where
  type RouteModel (NSumRoute '[]) = NP I '[]
  routeEncoder = mkRouteEncoder \Nil -> prism' (\case {}) (const Nothing)
  allRoutes Nil = []

type family GetModels (xs :: [Type]) :: [Type] where
  GetModels '[] = '[]
  GetModels (x : xs) = RouteModel x : GetModels xs

instance IsRoute () where
  type RouteModel () = ()
  routeEncoder = singletonRouteEncoder
  allRoutes () = [()]

type SingleRoute s = PrefixedRoute s ()

instance
  ( IsRoute x
  , IsRoute (NSumRoute xs)
  , RouteModel (NSumRoute xs) ~ NP I (GetModels xs)
  ) => IsRoute (NSumRoute (x : xs)) where
  type RouteModel (NSumRoute (x : xs)) = NP I (RouteModel x : GetModels xs)
  routeEncoder = combineRouteEncoder
                 (iso fromEither toEither % _Wrapped)
                 (\(I m :* ms) -> (m, ms))
                 routeEncoder
                 routeEncoder
    where
      fromEither :: Either x (NSumRoute xs) -> NS I (x : xs)
      fromEither (Left x) = Z (I x)
      fromEither (Right (NSumRoute xs)) = S xs
      toEither :: NS I (x : xs) -> Either x (NSumRoute xs)
      toEither (Z (I x)) = Left x
      toEither (S xs) = Right (NSumRoute xs)

  allRoutes (I x :* xs) = map (NSumRoute . Z . I) (allRoutes x) ++
                          map (\(NSumRoute rs) -> NSumRoute (S rs)) (allRoutes xs)

instance
  ( IsASumType r
  , IsRoute (NSumRoute (ASumCode r))
  , RouteModel (NSumRoute (ASumCode r)) ~ NP I (GetModels (ASumCode r))
  , NPNonEmpty (GetModels (ASumCode r))
  , IsProductType ref (FilterNonEmpty (GetModels (ASumCode r)))
  , Subtype ref m
  ) =>
  IsRoute (GenericRoute ref m r)
  where
  type RouteModel (GenericRoute ref m r) = m
  routeEncoder = mapRouteEncoder
                 equality
                 (_Unwrapped % re isASumTypeIso % _Wrapped)
                 (review npNonEmptyIso . productTypeFrom . upcast @ref)
                 (routeEncoder @(NSumRoute (ASumCode r)))
  allRoutes = map (view $ _Unwrapped % re isASumTypeIso % _Wrapped) .
              allRoutes @(NSumRoute (ASumCode r)) .
              (review npNonEmptyIso . productTypeFrom . upcast @ref)

subRouteEncoder ::
  forall ctor field m n i o.
  HasField' field m n =>
  AsConstructor' ctor i o =>
  m ->
  RouteEncoder m i ->
  RouteEncoder n o
subRouteEncoder m =
  mapRouteEncoder equality (_Ctor' @ctor) (flip (setField @field) m)
