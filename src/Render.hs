{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, BlockArguments #-}

-- |

module Render where

import qualified Models as M
import qualified Routes as R
import qualified Layouts as L
import qualified Style as S
import qualified Ema as E
import qualified Ema.CLI as ECli
import Data.Map (lookup, assocs)
import Data.Time (formatTime)
import Locale (ptTimeLocale)
import Lucid
import Models (StructuralPage(StructuralPage))

data SimplePage = SimplePage { simpleTitle :: Text, simpleBody :: Html () }

instance M.HtmlPage SimplePage where
  title = simpleTitle
  body  = simpleBody

applyDefaultLayout :: M.HtmlPage a => M.Model -> a -> E.Asset LByteString
applyDefaultLayout m = E.AssetGenerated E.Html . renderBS . L.primary m (L.head m) . M.body

pageOrnotFound :: M.HtmlPage a => M.Model -> Maybe a -> E.Asset LByteString
pageOrnotFound m (Just page) = applyDefaultLayout m page
pageOrnotFound m Nothing     = applyDefaultLayout m $ StructuralPage "404" "Esta página não existe :("

blogIndex :: M.Model -> SimplePage
blogIndex m = SimplePage "Blog" do
  h1_ "Blog"
  aside_ "Páginas mais curtas e mais frequentes."
  forM_ (reverse $ Data.Map.assocs $ M.blogPosts m) \(s,p) -> do
    h2_ $ a_ [href_ $ E.routeUrl m (R.BlogPage s)] (toHtmlRaw $ M.postTitle p)
    aside_ $ toHtml $ formatTime ptTimeLocale "%A, %e de %B de %Y" $ M.date p
    toHtmlRaw $ M.postBody p
    hr_ []

roamIndex :: M.Model -> SimplePage
roamIndex m = SimplePage "Zettelkasten" do
  h1_ "Zettelkasten"
  aside_ "My knowledge vault."
  forM_ (reverse $ Data.Map.assocs $ M.roamPosts m) \(uuid,p) -> do
    h2_ $ a_ [href_ $ E.routeUrl m (R.RoamPage uuid)] (toHtmlRaw $ M.postTitle p)
    aside_ $ toHtml $ formatTime ptTimeLocale "%A, %e de %B de %Y" $ M.date p

render :: ECli.Action -> M.Model -> R.Route -> E.Asset LByteString
render _ _ (R.Css "stylesheet")    = E.AssetGenerated E.Other (encodeUtf8 S.styleT)
render _ m (R.Css s)               = E.AssetStatic $ E.encodeRoute m (R.Css s)
render _ m (R.Js s)                = E.AssetStatic $ E.encodeRoute m (R.Js s)
render _ m (R.BlogAsset s i)       = E.AssetStatic $ E.encodeRoute m (R.BlogAsset s i)
render _ m (R.StructuralPage path) = pageOrnotFound m $ lookup path $ M.structuralPages m
render _ m (R.RoamEntryPoint)      = applyDefaultLayout m $ roamIndex m
render _ m (R.RoamPage uuid)       = pageOrnotFound m $ (,lookup uuid $ M.roamDatabase m)
                                                        <$> lookup uuid (M.roamPosts m)
render _ m (R.BlogPage slug)       = pageOrnotFound m $ lookup slug $ M.blogPosts m
render _ m  R.BlogIndex            = applyDefaultLayout m $ blogIndex m
