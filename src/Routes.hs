module Routes where

import qualified Models as M
import Ema (Slug)
import Data.UUID.Types (UUID)
import Path

type AssetID = Slug

data Route
  = StructuralPage Path
  | BlogIndex
  | BlogPage Slug
  | BlogAsset Slug AssetID
  | RoamEntryPoint
  | RoamPage UUID
  | TagsListing
  | TagPage Tag
  | Css Slug
  | Js Slug
  deriving (Show)

type Tag = String

home = StructuralPage [""]
sobre = StructuralPage ["sobre"]
monitoria = StructuralPage ["monitoria"]
feed = BlogIndex
artigos = StructuralPage ["artigos"]

topLevel =
  [ ("início", home)
  , ("feed", feed)
  , ("artigos", artigos)
  , ("monitoria", monitoria)
  , ("sobre", sobre)
  ]
