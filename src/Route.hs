{-# LANGUAGE UndecidableInstances #-}
-- |

module Route (Model (..), Route (..), ModelCache, cacheLens, cache0) where
import Ema
import Render (HeistS)
import Ema.Route.GenericClass
import qualified Generics.SOP as SOP
import qualified Site.Roam as R
import qualified Site.Static as S
import qualified Site.Content as C
import qualified Site.Roam.Model as R
import Optics.Core
import Data.Generics.Product
import Data.Binary (Binary)

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

data ModelCache = ModelCache
  { roamC :: R.ModelCache
  , contentC :: C.ModelCache
  }
  deriving (Generic, Binary)

cache0 :: ModelCache
cache0 = ModelCache R.cache0 C.cache0

cacheLens :: Lens' Model ModelCache
cacheLens = lens get_ set_
  where
    get_ m = ModelCache
      { roamC    = upcast $ getField @"roamM" m
      , contentC = upcast $ getField @"contentM" m
      }
    set_ m mc = m & over (field' @"roamM") (smash (roamC mc))
                  & over (field' @"contentM") (smash (contentC mc))
