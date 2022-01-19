-- |

module EmaInstance where

import Ema
import Routes
-- import qualified Models as M
import System.FilePath
import System.FilePattern ((?==))
import Path
import Locale
import Models (Model, Source, servePoint, staticAssets)

findAssetRoute :: Model -> FilePath -> Maybe Route
findAssetRoute m fp = exists' minBound
  where
    exists' :: Source -> Maybe Route
    exists' src
      | isPrefixOf point fp
        && relPath `isPathOfForest` staticAssets m src =
        Just $ StaticAsset src relPath
      | otherwise =
          if maxBound == src
          then Nothing
          else exists' (succ src)
      where
        point = servePoint src
        pointPath = urlToPath point
        relPath = drop (length pointPath) (urlToPath fp)

instance Ema Model Route where
  decodeRoute m fp =
    case fixedFp of
      "assets/css/stylesheet.css" -> Just StyleSheet
      "zettelkasten/index.html" -> Just RoamEntryPoint
      "zettelkasten/graph.json" -> Just RoamGraphJSON
      _
        | "zettelkasten/*/index.html" ?== fixedFp ->
          Just $ RoamPage (slugfy $ takeBaseName $ takeDirectory fixedFp)
        | isJust maybeAsset -> maybeAsset
        | "**/index.html" ?== fixedFp ->
          let (mbLocale, fp') = break (== '/') fixedFp
              page loc p = Just $ StructuralPage loc (urlToPath p)
          in case toLocale mbLocale of
            Just l -> page l (drop 1 fp')
            Nothing -> page defLocale fixedFp
        | otherwise -> Nothing
    where
      slugfy = decodeSlug . toText
      fixedFp = if hasExtension fp
                then fp
                else fp </> "index.html"
      maybeAsset = findAssetRoute m fixedFp


  encodeRoute _l = \case
    StyleSheet -> "assets/css/stylesheet.css"
    RoamEntryPoint -> "zettelkasten/index.html"
    RoamGraphJSON -> "zettelkasten/graph.json"
    RoamPage uuid -> "zettelkasten" /> tUnSlug uuid /> "index.html"
    StructuralPage loc path
      | loc == defLocale  -> pathToUrl path <.> "html"
      | otherwise         -> localeAbbrev loc /> pathToUrl path <.> "html"
    StaticAsset source path -> servePoint source /> pathToUrl path
    where
      tUnSlug = toString . unSlug


  allRoutes _model = []
