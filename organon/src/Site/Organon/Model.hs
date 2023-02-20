module Site.Organon.Model where

import Data.Aeson (Object)
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Site.Org ()
import Site.Org.Model qualified as O
import Site.Org.Render.Types (Layouts, OndimMS)
import Site.Organon.Cache (Cache)

type Anchor = Text

data TargetLocation = TargetLocation {locationPage :: O.UnresolvedLocation, locationAnchor :: Maybe Anchor}
  deriving (Eq, Ord, Show, Generic)

data Model = Model
  { org :: O.Model
  , static :: SR.Model
  , ondim :: OndimMS
  , layouts :: Layouts
  , cache :: TVar Cache
  , extraOpts :: Object
  , liveServer :: Bool
  , targetLocation :: Maybe TargetLocation
  }
  deriving (Generic)
