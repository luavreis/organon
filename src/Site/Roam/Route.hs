{-# LANGUAGE UndecidableInstances, PatternSynonyms #-}
-- |

module Site.Roam.Route (Route (..), pattern RoutePost', pattern RouteIndex', pattern RouteAttach', pattern RouteGraph') where
import Ema
import Ema.Route.Encoder
import Site.Roam.Model
import Ema.Route.GenericClass
import qualified Generics.SOP as SOP

data ModelI = MI {posts :: Map RoamID Post, attachments :: Set AttachPath}
  deriving (Generic, SOP.Generic)

data Route
  = RoutePost   (PrefixedRoute "post" RoamID)
  | RouteAttach (PrefixedRoute "attach" AttachPath)
  | RouteGraph  (SingleRoute "graph")
  | RouteIndex  ()
  deriving (Eq, Ord, Show, Generic, SOP.Generic)
  deriving (IsRoute) via (GenericRoute ModelI Model Route)

pattern RoutePost' :: RoamID -> Route
pattern RoutePost' x = RoutePost (PrefixedRoute x)

pattern RouteAttach' :: AttachPath -> Route
pattern RouteAttach' x = RouteAttach (PrefixedRoute x)

pattern RouteIndex' :: Route
pattern RouteIndex' = RouteIndex ()

pattern RouteGraph' :: Route
pattern RouteGraph' = RouteGraph (PrefixedRoute ())
