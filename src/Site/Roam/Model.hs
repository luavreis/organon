{-# LANGUAGE UndecidableInstances #-}

module Site.Roam.Model where
import JSON
import Org.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Relude.Extra (insert, delete, (!?), member)
import Render (HeistS)
import Routes
import Ema
import System.FilePath.Posix ((</>), splitDirectories)

data Model = Model
  { posts :: Map RoamID Post
  , database :: Database
  , fileAssoc :: Map FilePath (Set RoamID)
  , attachments :: Set AttachPath
  , attachDirs :: Map RoamID FilePath
  , mount :: FilePath
  , serveAt :: FilePath
  , heistS :: HeistS
  -- , modelCliAction :: Some Ema.CLI.Action
  }
  deriving (Generic)

newtype Post = Post
  { doc :: OrgDocument
  }
  deriving (Eq, Ord, Show)

newtype RoamID = RoamID Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, ToString, ToText, ToJSON, FromJSON)
  deriving (StringRoute) via Text
  deriving (IsRoute) via (MapRoute RoamID Post)

data AttachPath = AttachPath RoamID Text
  deriving stock (Eq, Ord, Show)
  deriving (IsRoute) via (SetRoute AttachPath)

instance StringRoute AttachPath where
  stringfy = (\(AttachPath rid txt) -> toString rid </> toString txt,
              \case
                (splitDirectories -> [fromString -> rid, fromString -> txt]) -> Just $ AttachPath rid txt
                _ -> Nothing)

data Backlink = Backlink
  { backlinkID :: RoamID
  , backlinkTitle :: [OrgInline]
  , backlinkExcerpt :: [OrgElement]
  }
  deriving (Eq, Ord, Show)

type Database = Map RoamID (Set Backlink)

-- | Delete backlinks made by a set of ids from the database.
deleteIdsFromRD :: Set RoamID -> Database -> Database
deleteIdsFromRD ks = Map.mapMaybeWithKey delFilter
  where
    delFilter _mentioned mentioners = -- Don't filter "mentioned" because we
                                      -- need to store links to nonexisting
                                      -- notes, as they may be added later
      case Set.filter (not . (`member` ks) . backlinkID) mentioners of
        xs | Set.null xs -> Nothing
           | otherwise -> Just xs

-- | Delete everything that was previously defined by a file, using information
-- from @fileAssoc@.
deleteAllFromFile :: FilePath -> Model -> Model
deleteAllFromFile fp m =
  case fileAssoc m !? fp of
    Just ids ->
      m { database = deleteIdsFromRD ids (database m)
        , posts = foldr delete (posts m) ids
        , fileAssoc = delete fp (fileAssoc m)
        }
    Nothing -> m

insertPost :: FilePath -> RoamID -> Post -> [(RoamID, Backlink)] -> Endo Model
insertPost fp k v blks =
  let filteredv = mapMaybe (\case (x, y) | x /= k -> Just (x, one y); _ -> Nothing) blks
      mappedv = Map.fromListWith (<>) filteredv
   in Endo \m -> m { posts = insert k v (posts m)
                   , database = Map.unionWith Set.union mappedv (database m)
                   , fileAssoc = Map.insertWith (<>) fp (one k) (fileAssoc m)
                   }

model0 :: Model
model0 = Model mempty mempty mempty mempty mempty "" "" Nothing
