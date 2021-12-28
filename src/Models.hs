{-# LANGUAGE BlockArguments, DeriveGeneric #-}
module Models where

import Ema (Slug, decodeSlug)
import Data.UUID.Types (UUID)
import qualified Data.Map as DM
import Lucid
import Data.Time (Day)
import Data.Binary.Instances.Time ()
import Data.Binary
import Data.Tree
import Relude.Extra.Map (delete, insert)
import qualified Data.UUID.Types as UUID

type Path = [Slug]

class HtmlPage a where
  title :: a -> Text
  body  :: a -> Html ()

data StructuralPage = StructuralPage { pageTitle :: Text, pageBody :: Text }
  deriving (Show, Generic)

instance HtmlPage StructuralPage where
  title = pageTitle
  body page = do
    h1_ (toHtmlRaw $ pageTitle page)
    toHtmlRaw $ pageBody page

data BlogPost = BlogPost { postTitle :: Text, postBody :: Text, date :: Day }
  deriving (Show, Generic)

instance HtmlPage BlogPost where
  title = postTitle
  body page = do
    h1_ (toHtmlRaw $ postTitle page)
    toHtmlRaw $ postBody page

newtype RawAssetId
  = RawHtmlId Slug
  deriving (Show,Eq,Ord,Generic)

data RoamBacklink = RoamBacklink
  { backlinkUUID :: UUID
  , backlinkTitle :: Text
  , backlinkExcerpt :: Text
  }
  deriving (Show, Generic)

-- | map (uuid of page x) -> list of (uuid, excerpt) of x's backlinks
-- The reason I'm using two maps is because they are built in somewhat different contexts
type RoamDatabase = Map UUID [RoamBacklink]

instance HtmlPage (BlogPost, [RoamBacklink]) where
  title = postTitle . fst
  body (page, backlinks) = do
    h1_ (toHtmlRaw $ postTitle page)
    toHtmlRaw $ postBody page
    hr_ []
    h2_ "Backlinks"
    forM_ backlinks $ \bl -> do
      a_ [href_ $ "zettelkasten/" <> UUID.toText (backlinkUUID bl)] do -- TODO link should not be hardcoded
        toHtmlRaw $ backlinkExcerpt bl

data Model = Model
  { name :: Text,
    fileStructure :: Tree Slug,
    structuralPages :: Map Path StructuralPage,
    blogPosts :: Map Slug BlogPost,
    rawAssets :: Map RawAssetId Text,
    roamPosts :: Map UUID BlogPost,
    roamDatabase :: RoamDatabase,
    roamAssoc :: Map Slug [UUID] }
  deriving (Show, Generic)

-- Probably I should use something like lens... I know nothing about it
insertRA :: RawAssetId -> Text -> Endo Model
insertRA k v = Endo \m -> m { rawAssets = insert k v (rawAssets m) }
deleteRA :: RawAssetId -> Endo Model
deleteRA k = Endo \m -> m { rawAssets = delete k (rawAssets m) }

insertSP :: [Slug] -> StructuralPage -> Endo Model
insertSP k v = Endo \m -> m { structuralPages = insert k v (structuralPages m) }
deleteSP :: [Slug] -> Endo Model
deleteSP k = Endo \m -> m { structuralPages = delete k (structuralPages m) }

insertBP :: Slug -> BlogPost -> Endo Model
insertBP k v = Endo \m -> m { blogPosts = insert k v (blogPosts m) }
deleteBP :: Slug -> Endo Model
deleteBP k = Endo \m -> m { blogPosts = delete k (blogPosts m) }

insertRP :: UUID -> BlogPost -> Endo Model
insertRP k v = Endo \m -> m { roamPosts = insert k v (roamPosts m) }
deleteRP :: UUID -> Endo Model
deleteRP k = Endo \m -> m { roamPosts = delete k (roamPosts m) }

mapDeleteRD :: [UUID] -> RoamDatabase -> RoamDatabase
mapDeleteRD ks = DM.mapMaybeWithKey delFilter
  where
    delFilter referent referees =
      if referent `elem` ks
      then Nothing
      else case filter (flip elem ks . backlinkUUID) referees of
        [] -> Nothing
        xs -> Just xs

addToRD :: [UUID] -> [(UUID, [RoamBacklink])] -> Endo Model
addToRD ks v =
  Endo \m -> m { roamDatabase = roamDatabase m
                                & mapDeleteRD ks
                                & DM.unionWith (++) (DM.fromListWith (++) v) }

deleteRD :: [UUID] -> Endo Model
deleteRD ks =
  Endo \m ->
    m { roamDatabase = mapDeleteRD ks (roamDatabase m) }

instance Binary Slug
instance Binary RawAssetId
instance Binary StructuralPage
instance Binary BlogPost
instance Binary RoamBacklink
instance Binary Model

defaultModel :: Model
defaultModel =
  Model
    "interseções"
    rootNode
    emptyMap
    emptyMap
    emptyMap
    emptyMap
    emptyMap
    emptyMap
  where
    rootNode = Node (decodeSlug "") []
    emptyMap = DM.empty
