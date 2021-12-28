module Routes where

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

home :: Route
home = StructuralPage [""]

sobre :: Route
sobre = StructuralPage ["sobre"]

monitoria :: Route
monitoria = StructuralPage ["monitoria"]

feed :: Route
feed = BlogIndex

zettel :: Route
zettel = RoamEntryPoint

artigos :: Route
artigos = StructuralPage ["artigos"]

topLevel :: [([Char], Route)]
topLevel =
  [ ("início", home)
  , ("feed", feed)
  , ("zettelkasten", zettel)
  , ("artigos", artigos)
  , ("monitoria", monitoria)
  , ("sobre", sobre)
  ]
