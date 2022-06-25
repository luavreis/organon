{-# LANGUAGE UndecidableInstances, PatternSynonyms #-}
-- |

module Site.Roam.Route (Route (..), pattern RoutePost', pattern RouteIndex', pattern RouteGraph') where
import Ema ( IsRoute, PrefixedRoute(PrefixedRoute) )
import Ema.Route.Encoder ( RouteEncoder(RouteEncoder) )
import Site.Roam.Model ( RoamID, Post, Model )
import Ema.Route.GenericClass ( GenericRoute(GenericRoute), SingleRoute )
import Generics.SOP qualified as SOP
import OrgAttach (AttachModel, AttachRoute)

data ModelI = MI {posts :: Map RoamID Post, attachments :: AttachModel}
  deriving (Generic, SOP.Generic)

data Route
  = RoutePost   (PrefixedRoute "post" RoamID)
  | RouteAttach AttachRoute -- FIXME for now we can't prefix this :(
  | RouteGraph  (SingleRoute "graph")
  | RouteIndex  ()
  deriving (Eq, Show, Generic, SOP.Generic)
  deriving (IsRoute) via (GenericRoute ModelI Model Route)

pattern RoutePost' :: RoamID -> Route
pattern RoutePost' x = RoutePost (PrefixedRoute x)

pattern RouteIndex' :: Route
pattern RouteIndex' = RouteIndex ()

pattern RouteGraph' :: Route
pattern RouteGraph' = RouteGraph (PrefixedRoute ())
