module Routes where

import Ema (Slug)
import Path
import Locale
import Models

data Route
  = StructuralPage Locale Path
  | RoamEntryPoint
  | RoamGraphJSON
  | RoamPage UUID
  | StyleSheet
  | StaticAsset Source [Slug]
  deriving (Show)

type Tag = String
