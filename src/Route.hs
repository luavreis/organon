{-# LANGUAGE UndecidableInstances #-}
-- |

module Route (Model (..), Route (..)) where
import Ema
import Render (HeistS)
import Ema.Route.GenericClass
import qualified Generics.SOP as SOP
import qualified Site.Roam as R
import qualified Site.Static as S
import qualified Site.Content as C

data ModelI = MI { roamM :: R.Model, staticM :: S.Model, contentM :: C.Model }
  deriving (Generic, SOP.Generic)

data Route
  = RouteRoam R.RoamRoute
  | RouteStatic S.StaticRoute
  | RouteContent C.ContentRoute
  deriving (Eq, Show, Generic, SOP.Generic)
  deriving (IsRoute) via (GenericRoute ModelI Model Route)

data Model = Model
  { roamM :: R.Model
  , staticM :: S.Model
  , contentM :: C.Model
  , heistS :: HeistS
  }
  deriving (Generic)
