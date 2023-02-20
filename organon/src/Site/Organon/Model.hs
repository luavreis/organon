module Site.Organon.Model (
  Model (..),
  TargetLocation (..),
  Anchor,
) where

import Data.Aeson (Object)
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Site.Org.Model qualified as Org
import Site.Org.Render.Types
import Site.Organon.Cache

type Anchor = Text

data TargetLocation = TargetLocation {locationPage :: Org.UnresolvedLocation, locationAnchor :: Maybe Anchor}
  deriving (Eq, Ord, Show, Generic)

data Model = Model
  { org :: Org.Model
  , static :: SR.Model
  , ondim :: OndimMS
  , layouts :: Layouts
  , cache :: TVar Cache
  , extraOpts :: Object
  , liveServer :: Bool
  , targetLocation :: Maybe TargetLocation
  }
  deriving (Generic)
