-- |

module EmaInstance where

import Ema
import Routes
import System.FilePath
import System.FilePattern ((?==))
import Path
import Locale
import Relude.Extra.Map (lookup)
import Models hiding (StructuralPage)
import qualified Data.Map as Map


findAssetRoute :: Model -> FilePath -> Maybe Route
findAssetRoute m fp = exists' minBound
  where
    exists' :: Source -> Maybe Route
    exists' src
      | isPrefixOf point fp
        && relPath `isPathOfForest` lookup src (staticAssets m) =
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
  decodeRoute m fp = case fp of
      "assets/css/stylesheet.css" -> Just StyleSheet
      "zettelkasten/index.html" -> Just RoamEntryPoint
      "zettelkasten/graph.json" -> Just RoamGraphJSON
      _
        | "zettelkasten/*/index.html" ?== fp ->
          Just $ RoamPage (slugfy $ takeBaseName $ takeDirectory fp)
        | isJust maybeAsset -> maybeAsset
        | "**/index.html" ?== fp ->
          let (mbLocale, fp') = break (== '/') fp
              page loc p = Just $ StructuralPage loc (urlToPath p)
          in case toLocale mbLocale of
            Just l -> page l (drop 1 fp')
            Nothing -> page defLocale fp
        | otherwise -> Nothing
    where
      slugfy = decodeSlug . toText
      maybeAsset = findAssetRoute m fp


  encodeRoute _l = \case
    StyleSheet -> "assets/css/stylesheet.css"
    RoamEntryPoint -> "zettelkasten/index.html"
    RoamGraphJSON -> "zettelkasten/graph.json"
    RoamPage uuid -> "zettelkasten" </> tUnSlug uuid </> "index"
    StructuralPage loc path
      | loc == defLocale  -> pathToUrl path
      | otherwise         -> localeAbbrev loc </> pathToUrl path
    StaticAsset source path -> servePoint source </> pathToUrl path
    where
      tUnSlug = toString . unSlug


  allRoutes m =
    [StyleSheet, RoamEntryPoint, RoamGraphJSON]
    <> [StaticAsset src path | (src, tree) <- Map.assocs (staticAssets m)
                             , path <- allLeafs tree]
    <> [RoamPage uuid | uuid <- Map.keys (roamPosts m)]
    <> [StructuralPage loc slug | (slug, localized) <- Map.assocs (structuralPages m)
                                , loc <- Map.keys localized ]
