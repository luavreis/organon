{-# LANGUAGE UndecidableInstances #-}

module Site.Roam.Model where
import Ema
import Optics.Core
import Ema.Route.Encoder
import Org.Types
import Generics.SOP qualified as SOP
import qualified Data.Map as Map
import qualified Data.Set as Set
import Relude.Extra (insert, delete, (!?), keys, member)
import System.FilePath (stripExtension)

data Model = Model
  { posts :: Map RoamID Post
  , database :: Database
  , fileAssoc :: Map FilePath [RoamID]
  -- , modelCliAction :: Some Ema.CLI.Action
  }
  deriving stock (Show, Generic)

data Post = Post
  { doc :: OrgDocument
  , tags :: [Text]
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

initialModel :: Model
initialModel = Model mempty mempty mempty

newtype RoamID = RoamID Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, ToString, ToText)

instance IsRoute RoamID where
  type RouteModel RoamID = Model
  routeEncoder = mkRouteEncoder \m ->
    prism' (<> ".html") (\fp -> do
        rid <- fromString <$> stripExtension ".html" fp
        guard (rid `member` posts m) $> rid
      )
    % iso id toString
  allRoutes m = keys (posts m)

data RoamRoute
  = RoamRoute_Post RoamID
  | RoamRoute_Index
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsRoute) via (SingleModelRoute Model RoamRoute)

newtype BadRoute = BadRoute RoamRoute
  deriving stock (Show)
  deriving anyclass (Exception)
