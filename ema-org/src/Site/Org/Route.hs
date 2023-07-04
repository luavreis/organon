{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Site.Org.Route where

import Ema.Route.Class (IsRoute)
import Ema.Route.Generic (
  GenericRoute (..),
  HasSubModels,
  HasSubRoutes,
  WithModel,
  WithSubRoutes,
 )
import Generics.SOP qualified as SOP
import Site.Org.Model (Identifier, Model, StaticFile)

data Route
  = Route_Page Identifier
  | Route_Static StaticFile
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithModel Model
             , WithSubRoutes
                '[ Identifier
                 , StaticFile
                 ]
             ]
        )
