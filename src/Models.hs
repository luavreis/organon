{-# LANGUAGE BlockArguments, DeriveGeneric #-}
module Models where

import Ema (Slug, decodeSlug)
import Data.UUID.Types (UUID)
import Data.Map as DM (empty)
import Lucid (Html, toHtmlRaw, h1_)
import Data.Time (Day)
import Data.Binary.Instances.Time
import Data.Binary
import Data.Tree

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

-- | (map uuid -> page, map (uuid of page x) -> list of (uuid, excerpt) of x's backlinks)
-- The reason I'm using two maps is because they are built in somewhat different contexts
type RoamDatabase = (Map UUID BlogPost, Map UUID [(UUID, Text)])

data Model = Model
  { name :: Text,
    fileStructure :: Tree Slug,
    structuralPages :: Map Path StructuralPage,
    blogPosts :: Map Slug BlogPost,
    rawAssets :: Map RawAssetId Text,
    roamDatabase :: RoamDatabase }
  deriving (Show, Generic)

instance Binary Slug
instance Binary RawAssetId
instance Binary StructuralPage
instance Binary BlogPost
instance Binary Model

defaultModel :: Model = Model "interseções" rootNode empty empty empty (empty, empty)
  where
    rootNode = Node (decodeSlug "") []
    empty = DM.empty
