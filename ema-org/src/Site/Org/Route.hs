{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Site.Org.Route where

import Ema.Route.Class
import Ema.Route.Generic
import Generics.SOP qualified as SOP
import Site.Org.Model

data Route
  = Route_Page Identifier
  | Route_Static StaticFileIx
  | Route_Graph
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithModel Model
             , WithSubRoutes
                '[ Identifier
                 , StaticFileIx
                 , FileRoute "graph.json"
                 ]
             ]
        )
