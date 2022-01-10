module Routes where

import Ema (Slug)
import Data.UUID.Types (UUID)
import Path
import Locale

type AssetID = Slug

data Route
  = StructuralPage Locale Path
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
