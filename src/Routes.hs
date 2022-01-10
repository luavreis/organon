module Routes where

import Ema (Slug)
import Path
import Locale
import Models (UUID)

type AssetID = Slug

data Route
  = StructuralPage Locale Path
  | BlogIndex
  | BlogPage Slug
  | BlogAsset Slug AssetID
  | RoamEntryPoint
  | RoamGraphJSON
  | RoamPage UUID
  | TagsListing
  | TagPage Tag
  | Css Slug
  | Js Slug
  deriving (Show)

type Tag = String
