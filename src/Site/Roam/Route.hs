{-# LANGUAGE UndecidableInstances, PatternSynonyms #-}
-- |

module Site.Roam.Route (Route (..), pattern RoutePost', pattern RouteIndex', pattern RouteAttach', pattern RouteGraph') where
import Ema
import Ema.Route.Encoder
import Optics.Core
import Site.Roam.Model
import Ema.Route.GenericClass
import System.FilePath
import Relude.Extra
import qualified Generics.SOP as SOP

instance IsRoute RoamID where
  type RouteModel RoamID = Map RoamID Post
  routeEncoder = mkRouteEncoder \m ->
    prism' (<> ".html") (\fp -> do
        rid <- fromString <$> stripExtension ".html" fp
        guard (rid `member` m) $> rid
      )
    % iso id toString
  allRoutes m = keys m

instance IsRoute AttachPath where
  type RouteModel AttachPath = Set AttachPath
  routeEncoder = mkRouteEncoder \m ->
    prism'
      (\(AttachPath rid txt) ->
         toString rid </> toString txt)
      (\case
          (splitDirectories -> [fromString -> rid, fromString -> txt]) ->
            let att = AttachPath rid txt
            in guard (att `member` m) $> att
          _ -> Nothing)
  allRoutes m = toList m

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
