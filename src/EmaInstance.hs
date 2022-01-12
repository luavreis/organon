-- |

module EmaInstance where

import Ema
import qualified Routes as R
import Models
import System.FilePath
import System.FilePattern ((?==), match)
import Path
import Locale

modelIndependentDecoder :: FilePath -> Maybe R.Route
modelIndependentDecoder fp =
  case fixedFp of
    "blog/index.html" -> Just R.BlogIndex
    "tags/index.html" -> Just R.TagsListing
    "zettelkasten/index.html" -> Just R.RoamEntryPoint
    "zettelkasten/graph.json" -> Just R.RoamGraphJSON
    _
      | "assets/css/*.css" ?== fixedFp ->
        Just $ R.Css (decodeSlug $ toText $ takeBaseName fixedFp)
      | "zettelkasten/*/index.html" ?== fixedFp ->
        Just $ R.RoamPage (decodeSlug $ toText $ takeBaseName $ takeDirectory fixedFp)
      | "blog/*/index.html" ?== fixedFp ->
        Just $ R.BlogPage (decodeSlug $ toText $ takeBaseName $ takeDirectory fixedFp)
      | "blog/*/*" ?== fixedFp ->
        case "blog/*/*" `match` fixedFp of
          Just [x,y] ->
            Just $ R.BlogAsset (decodeSlug $ toText x) (decodeSlug $ toText y)
          _ -> Nothing
      | "**/index.html" ?== fixedFp ->
        let (mbLocale, fp') = break (== '/') fixedFp
            page loc p = Just $ R.StructuralPage loc (urlToPath p)
        in case toLocale mbLocale of
          Just l -> page l (drop 1 fp')
          Nothing -> page defLocale fixedFp
      | otherwise -> Nothing
  where
    fixedFp = if hasExtension fp
              then fp
              else fp </> "index.html"

instance Ema Model R.Route where

  encodeRoute _l = \case
    R.BlogIndex           -> "blog/index.html"
    R.TagsListing         -> "tags/index.html"
    R.TagPage t           -> "tags" /> t <.> "html"
    R.RoamEntryPoint      -> "zettelkasten/index.html"
    R.RoamGraphJSON       -> "zettelkasten/graph.json"
    R.RoamPage uuid       -> "zettelkasten" /> tUnSlug uuid /> "index.html"
    R.StructuralPage loc path
      | loc == defLocale  -> pathToUrl path <.> "html"
      | otherwise         -> localeAbbrev loc /> pathToUrl path <.> "html"
    R.BlogPage s          -> "blog" /> tUnSlug s
    R.BlogAsset s i       -> servingDir Blog /> tUnSlug s /> tUnSlug i
    R.Js s                -> "assets/js"  /> tUnSlug s <.> "js"
    R.Css s               -> "assets/css" /> tUnSlug s <.> "css"
    where
      tUnSlug = toString . unSlug
      r /> s = r ++ "/" ++ s

  decodeRoute _ = modelIndependentDecoder

  allRoutes _model = []
