-- |

module EmaInstance where

import Ema
import qualified Routes as R
import qualified Data.UUID.Types as UUID
import Models
import System.FilePath
import System.FilePattern ((?==), match)
import Path

instance Ema Model R.Route where
  encodeRoute _model = \case
    R.BlogIndex           -> "blog/index.html"
    R.TagsListing         -> "tags/index.html"
    R.TagPage t           -> "tags/" ++ t ++ ".html"
    R.RoamEntryPoint      -> "zettelkasten/index.html"
    R.RoamPage uuid       -> "zettelkasten/" ++ UUID.toString uuid ++ "/index.html"
    R.StructuralPage path -> pathToUrl path <> ".html"
    R.BlogPage s          -> toString $ "blog/" <> unSlug s
    R.BlogAsset s i       -> toString $ "blog/" <> unSlug s <> "/" <> unSlug i
    R.Js s                -> toString $ "assets/js/"  <> unSlug s <> ".js"
    R.Css s               -> toString $ "assets/css/" <> unSlug s <> ".css"
  decodeRoute _ fp =
    let fixedFp =
          if hasExtension fp
          then fp
          else fp </> "index.html"
    in case fixedFp of
      "blog/index.html" -> Just R.BlogIndex
      "tags/index.html" -> Just R.TagsListing
      "zettelkasten/index.html" -> Just R.RoamEntryPoint
      _
        | "assets/css/*.css" ?== fixedFp ->
          Just $ R.Css (decodeSlug $ toText $ takeBaseName fixedFp)
        | "zettelkasten/*/index.html" ?== fixedFp ->
          R.RoamPage <$> (UUID.fromString $ takeBaseName $ takeDirectory fixedFp)
        | "blog/*/index.html" ?== fixedFp ->
          Just $ R.BlogPage (decodeSlug $ toText $ takeBaseName $ takeDirectory fixedFp)
        | "blog/*/*" ?== fixedFp ->
          case "blog/*/*" `match` fixedFp of
            Just [x,y] ->
              Just $ R.BlogAsset (decodeSlug $ toText x) (decodeSlug $ toText y)
            _ -> Nothing
        | "**/index.html" ?== fixedFp ->
          Just $ R.StructuralPage $ urlToPath fixedFp
        | otherwise -> Nothing
  allRoutes _model = []
