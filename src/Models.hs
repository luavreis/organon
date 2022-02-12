{-# LANGUAGE BlockArguments #-}
module Models where

import Ema
import Path
import Data.Map ( (!?), delete, insert, alter )
import System.FilePath
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lucid
import Data.Time (Day)
import Data.Binary.Instances.Time ()
import Data.Binary
import Locale

instance Binary Slug

type UUID = Slug

data Source
  = Asset
  | Zettel
  | Content
  deriving ( Eq
           , Ord
           , Show
           , Enum
           , Bounded
           , Generic
           , Binary
           )

mountPoint :: IsString p => Source -> p
mountPoint Zettel = "/home/lucas/dados/notas"
mountPoint Asset = "assets"
mountPoint Content = "content"

servePoint :: IsString p => Source -> p
servePoint Zettel = "zettelkasten"
servePoint Asset = "assets"
servePoint Content = ""

type Place = (Source, FilePath)

filepath :: Place -> FilePath
filepath (src, fp) = mountPoint src </> fp

servepath :: Place -> FilePath
servepath (src, fp) = servePoint src </> fp

data Model = Model
  { siteName :: Text
  , staticAssets :: Map Source (Tree Slug)
  , structuralPages :: Map Path (Localized StructuralPage)
  , layouts :: Map String Text
  , roamPosts :: Map UUID RoamPost
  , roamDatabase :: RoamDatabase
  , roamAssoc :: Map FilePath [UUID]
  } deriving (Generic)

type RoamDatabase = Map UUID (Set RoamBacklink)

class HtmlPage a where
  title :: a -> Text
  body  :: a -> Html ()

data StructuralPage = StructuralPage
  { pageTitle :: Text
  , pageBody :: Text
  } deriving (Generic, Eq, Ord, Binary)

instance HtmlPage StructuralPage where
  title = pageTitle
  body page = do
    h1_ (toHtmlRaw $ pageTitle page)
    toHtmlRaw $ pageBody page

type Tag = Slug

data RoamPost = RoamPost
  { postTitle :: Text
  , postBody :: Text
  , postDate :: Day
  , postTags :: [Tag]
  } deriving (Generic, Eq, Ord, Binary)

data RoamBacklink = RoamBacklink
  { backlinkUUID :: UUID
  , backlinkTitle :: Text
  , backlinkExcerpt :: Text
  } deriving (Eq, Ord, Generic, Binary)

instance HtmlPage (RoamPost, Maybe (Set RoamBacklink)) where
  title = postTitle . fst
  body (page, mbacklinks) = do
    h1_ (toHtmlRaw $ postTitle page)
    toHtmlRaw $ postBody page
    case mbacklinks of
      Just backlinks -> do
        hr_ []
        h5_ $ "Páginas com referências a esta página (" <> show (length backlinks) <> "):"
        forM_ backlinks $ \bl -> do
          div_ [class_ "backlink",
                onclick_ $ "window.location.href = 'zettelkasten/" <> encodeSlug (backlinkUUID bl) <> "'",
                style_ "font-size: 90%;\
                       \color: #444;\
                       \border-left: solid 2px #ccc;\
                       \cursor: pointer;\
                       \padding-left: 0.7em;\
                       \margin: 10px 0px;\
                       \padding-top: 3px;\
                       \border-radius: 5px;"] do
            a_ [style_ "font-weight: 500; font-size: 26px; text-decoration: none; background: #dabce488", href_ $ "zettelkasten/" <> encodeSlug (backlinkUUID bl)] $ -- TODO link should not be hardcoded
              toHtmlRaw $ backlinkTitle bl
            toHtmlRaw $ backlinkExcerpt bl
      Nothing -> do
        hr_ []
        i_ "Nenhuma outra página faz referências a esta página."

-- Probably I should use something like lens... I know nothing about it
insertL :: String -> Text -> Endo Model
insertL k v = Endo \m -> m { layouts = insert k v (layouts m) }
deleteL :: String -> Endo Model
deleteL k = Endo \m -> m { layouts = delete k (layouts m) }

insertSA :: Source -> FilePath -> Endo Model
insertSA src fp = Endo \m ->
  m { staticAssets = alter (forestInsert (urlToPath fp)) src (staticAssets m) }

deleteSA :: Source -> FilePath -> Endo Model
deleteSA src fp = Endo \m ->
  m { staticAssets = alter (forestDelete (urlToPath fp)) src (staticAssets m) }

insertSP :: Path -> Locale -> StructuralPage -> Endo Model
insertSP k l v = Endo \m -> m { structuralPages = Map.insertWith Map.union k (Map.singleton l v) (structuralPages m) }
deleteSP :: Path -> Locale -> Endo Model
deleteSP k l = Endo \m -> m { structuralPages = Map.adjust (delete l) k (structuralPages m) }

insertRP :: UUID -> RoamPost -> Endo Model
insertRP k v = Endo \m -> m { roamPosts = insert k v (roamPosts m) }

mapDeleteRD :: [UUID] -> RoamDatabase -> RoamDatabase
mapDeleteRD ks = Map.mapMaybeWithKey delFilter
  where
    delFilter _refereed referents =
      case Set.filter (not . flip elem ks . backlinkUUID) referents of
        xs | Set.null xs -> Nothing
           | otherwise -> Just xs

mapFilterRD :: FilePath -> [UUID] -> Endo Model
mapFilterRD fp ks =
  Endo \m ->
    case roamAssoc m !? fp of
      Just pks ->
        let cks = filter (`notElem` ks) pks
        in m { roamDatabase = mapDeleteRD cks (roamDatabase m)
             , roamPosts = foldr delete (roamPosts m) cks
             , roamAssoc = insert fp ks (roamAssoc m)
             }
      Nothing -> m { roamAssoc = insert fp ks (roamAssoc m) }

addToRD :: UUID -> [(UUID, RoamBacklink)] -> Endo Model
addToRD k v =
  let filteredv = mapMaybe
                  (\case (x, y) | k /= x -> Just (x, [y]); _ -> Nothing) v
      mappedv = Map.map Set.fromList $ Map.fromListWith (++) filteredv
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

defaultModel :: Model
defaultModel =
  Model
    "sempre-viva"
    mempty
    emptyMap
    emptyMap
    emptyMap
    emptyMap
    emptyMap
  where
    emptyMap = Map.empty
