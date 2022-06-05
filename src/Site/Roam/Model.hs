{-# LANGUAGE UndecidableInstances #-}

module Site.Roam.Model where
import Ema
import JSON
import Optics.Core
import Ema.Route.Encoder
import Org.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Relude.Extra (insert, delete, (!?), keys, member)
import System.FilePath (stripExtension, (</>), splitDirectories)
import Heist (HeistState)
import Org.Exporters.Heist (Exporter)
import Generics.SOP qualified as SOP

data Model = Model
  { posts :: Map RoamID Post
  , database :: Database
  , fileAssoc :: Map FilePath [RoamID]
  , attachments :: Set AttachPath
  , attachDirs :: Map RoamID FilePath
  , mount :: FilePath
  , serveAt :: FilePath
  , hState :: Maybe (HeistState Exporter)
  -- , modelCliAction :: Some Ema.CLI.Action
  }
  deriving (Generic)

model0 :: Model
model0 = Model mempty mempty mempty mempty mempty "" "" Nothing

newtype Post = Post
  { doc :: OrgDocument
  }
  deriving (Eq, Ord, Show)

data Backlink = Backlink
  { backlinkID :: RoamID
  , backlinkTitle :: [OrgInline]
  , backlinkExcerpt :: [OrgElement]
  }
  deriving (Eq, Ord, Show)

type Database = Map RoamID (Set Backlink)

deleteIdsFromRD :: [RoamID] -> Database -> Database
deleteIdsFromRD ks = Map.mapMaybeWithKey delFilter
  where
    delFilter _refereed referents =
      case Set.filter (not . flip elem ks . backlinkID) referents of
        xs | Set.null xs -> Nothing
           | otherwise -> Just xs

filterIds :: FilePath -> [RoamID] -> Endo Model
filterIds fp ks =
  Endo \m ->
    case fileAssoc m !? fp of
      Just pks ->
        let cks = filter (`notElem` ks) pks
        in m { database = deleteIdsFromRD cks (database m)
             , posts = foldr delete (posts m) cks
             , fileAssoc = insert fp ks (fileAssoc m)
             }
      Nothing -> m { fileAssoc = insert fp ks (fileAssoc m) }

deleteRD :: FilePath -> Model -> Model
deleteRD fp m =
  case fileAssoc m !? fp of
    Just ks -> m { database = deleteIdsFromRD ks (database m)
                 , posts = foldr delete (posts m) ks
                 , fileAssoc = delete fp (fileAssoc m)
                 }
    Nothing -> m

insertPost :: RoamID -> Post -> [(RoamID, Backlink)] -> Endo Model
insertPost k v blks =
  let filteredv = mapMaybe
                  (\case (x, y) | k /= x -> Just (x, [y]); _ -> Nothing) blks
      mappedv = Map.map Set.fromList $ Map.fromListWith (++) filteredv
   in Endo \m -> m { posts = insert k v (posts m)
                   , database = database m
                               & deleteIdsFromRD [k]
                               & Map.unionWith Set.union mappedv
                   }

newtype RoamID = RoamID Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, ToString, ToText, ToJSON, FromJSON)

instance IsRoute RoamID where
  type RouteModel RoamID = Model
  routeEncoder = mkRouteEncoder \m ->
    prism' (<> ".html") (\fp -> do
        rid <- fromString <$> stripExtension ".html" fp
        guard (rid `member` posts m) $> rid
      )
    % iso id toString
  allRoutes m = keys (posts m)

data AttachPath = AttachPath RoamID Text
  deriving stock (Eq, Ord, Show)

instance IsRoute AttachPath where
  type RouteModel AttachPath = Model
  routeEncoder = mkRouteEncoder \m ->
    prism'
      (\(AttachPath rid txt) ->
         toString rid </> toString txt)
      (\case
          (splitDirectories -> [fromString -> rid, fromString -> txt]) ->
            let att = AttachPath rid txt
            in guard (att `member` attachments m) $> att
          _ -> Nothing)
  allRoutes m = toList (attachments m)

data Route
  = Route_Post RoamID
  | Route_Attach AttachPath
  | Route_Graph
  | Route_Index
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model Route)

newtype BadRoute = BadRoute Route
  deriving stock (Show)
  deriving anyclass (Exception)
