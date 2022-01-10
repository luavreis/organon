{-# LANGUAGE BlockArguments, DeriveGeneric #-}
module Models where

import Ema (Slug, decodeSlug)
import Data.UUID.Types (UUID)
import Data.Map ((!?))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lucid
import Data.Time (Day)
import Data.Binary.Instances.Time ()
import Data.Binary
import Data.Tree
import Relude.Extra.Map (delete, insert)
import qualified Data.UUID.Types as UUID
import Locale

type Path = [Slug]

data Source
  = Layout
  | Blog
  | Content
  | Zettel
  deriving (Eq,Ord,Show,Enum,Bounded,Generic)

instance Binary Source

mountPoint :: IsString p => Source -> p
mountPoint Zettel = "/home/lucas/Lucas/notas"
mountPoint Blog = "/home/lucas/Lucas/blog"
mountPoint Content = "content"
mountPoint Layout = "assets/html"

servingDir :: IsString p => Source -> p
servingDir Zettel = "zettelkasten"
servingDir Blog = "blog"
servingDir Content = ""
servingDir Layout = ""

mountSet :: Set (Source, FilePath)
mountSet = Set.fromList [(s, mountPoint s) | s <- [Layout, Content, Blog, Zettel]]


class HtmlPage a where
  title :: a -> Text
  body  :: a -> Html ()

data StructuralPage = StructuralPage { pageTitle :: Text, pageBody :: Text }
  deriving (Generic, Eq, Ord)

instance HtmlPage StructuralPage where
  title = pageTitle
  body page = do
    h1_ (toHtmlRaw $ pageTitle page)
    toHtmlRaw $ pageBody page

data BlogPost = BlogPost { postTitle :: Text, postBody :: Text, date :: Day }
  deriving (Generic, Eq, Ord)

instance HtmlPage BlogPost where
  title = postTitle
  body page = do
    h1_ (toHtmlRaw $ postTitle page)
    toHtmlRaw $ postBody page

data RoamBacklink = RoamBacklink
  { backlinkUUID :: UUID
  , backlinkTitle :: Text
  , backlinkExcerpt :: Text
  }
  deriving (Eq, Ord, Generic)

-- | map (uuid of page x) -> list of (uuid, excerpt) of x's backlinks
type RoamDatabase = Map UUID (Set RoamBacklink)

instance HtmlPage (BlogPost, Maybe (Set RoamBacklink)) where
  title = postTitle . fst
  body (page, mbacklinks) = do
    h1_ (toHtmlRaw $ postTitle page)
    toHtmlRaw $ postBody page
    h2_ "Backlinks"
    case mbacklinks of
      Just backlinks ->
        forM_ backlinks $ \bl -> do
          h3_ $ a_ [href_ $ "zettelkasten/" <> UUID.toText (backlinkUUID bl)] $ -- TODO link should not be hardcoded
            toHtmlRaw $ backlinkTitle bl
          toHtmlRaw $ backlinkExcerpt bl
      Nothing -> p_ $ i_ "No backlinks."

data Model = Model
  { name :: Text,
    fileStructure :: Tree Slug,
    structuralPages :: Map Path (Localized StructuralPage),
    blogPosts :: Map Slug BlogPost,
    layouts :: Map String Text,
    roamPosts :: Map UUID BlogPost,
    roamDatabase :: RoamDatabase,
    roamAssoc :: Map FilePath [UUID] }
  deriving (Generic)

-- Probably I should use something like lens... I know nothing about it
insertL :: String -> Text -> Endo Model
insertL k v = Endo \m -> m { layouts = insert k v (layouts m) }
deleteL :: String -> Endo Model
deleteL k = Endo \m -> m { layouts = delete k (layouts m) }

insertSP :: Path -> Locale -> StructuralPage -> Endo Model
insertSP k l v = Endo \m -> m { structuralPages = Map.insertWith Map.union k (Map.singleton l v) (structuralPages m) }
deleteSP :: Path -> Locale -> Endo Model
deleteSP k l = Endo \m -> m { structuralPages = Map.adjust (delete l) k (structuralPages m) }

insertBP :: Slug -> BlogPost -> Endo Model
insertBP k v = Endo \m -> m { blogPosts = insert k v (blogPosts m) }
deleteBP :: Slug -> Endo Model
deleteBP k = Endo \m -> m { blogPosts = delete k (blogPosts m) }

insertRP :: UUID -> BlogPost -> Endo Model
insertRP k v = Endo \m -> m { roamPosts = insert k v (roamPosts m) }
deleteRP :: UUID -> Endo Model
deleteRP k = Endo \m -> m { roamPosts = delete k (roamPosts m) }

insertRA :: FilePath -> [UUID] -> Endo Model
insertRA k v = Endo \m -> m { roamAssoc = insert k v (roamAssoc m) }

mapDeleteRD :: [UUID] -> RoamDatabase -> RoamDatabase
mapDeleteRD ks = Map.mapMaybeWithKey delFilter
  where
    delFilter _referee referents =
      case Set.filter (not . flip elem ks . backlinkUUID) referents of
        xs | Set.null xs -> Nothing
           | otherwise -> Just xs

addToRD :: UUID -> [(UUID, [RoamBacklink])] -> Endo Model
addToRD k v =
  let mappedv = Map.map Set.fromList $ Map.fromListWith (++) v
  in Endo \m -> m { roamDatabase = roamDatabase m
                                   & mapDeleteRD [k]
                                   & Map.unionWith Set.union mappedv}

deleteFromRD :: FilePath -> Endo Model
deleteFromRD fp =
  Endo \m ->
    case roamAssoc m !? fp of
      Just ks -> m { roamDatabase = mapDeleteRD ks (roamDatabase m)
                   , roamPosts = foldr delete (roamPosts m) ks
                   , roamAssoc = delete fp (roamAssoc m)
                   }
      Nothing -> m

instance Binary Slug
instance Binary StructuralPage
instance Binary BlogPost
instance Binary RoamBacklink
instance Binary Locale
instance Binary Model

defaultModel :: Model
defaultModel =
  Model
    ""
    rootNode
    emptyMap
    emptyMap
    emptyMap
    emptyMap
    emptyMap
    emptyMap
  where
    rootNode = Node (decodeSlug "") []
    emptyMap = Map.empty
