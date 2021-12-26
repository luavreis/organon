{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, BlockArguments #-}

-- |

module Render where

import qualified Models as M
import qualified Routes as R
import qualified Layouts as L
import qualified Style as S
import qualified Ema as E
import qualified Ema.CLI as ECli
import qualified Data.Map
import qualified Relude.Extra.Map as Map
import Data.Time (formatTime)
import Data.Maybe (fromMaybe)
import Locale (ptTimeLocale)
import Lucid
import Path
import Models (StructuralPage(StructuralPage))

data SimplePage = SimplePage { simpleTitle :: Text, simpleBody :: Html () }

instance M.HtmlPage SimplePage where
  title = simpleTitle
  body  = simpleBody

applyDefaultLayout :: M.HtmlPage a => M.Model -> a -> E.Asset LByteString
applyDefaultLayout m = E.AssetGenerated E.Html . renderBS . L.primary m (L.head m) . M.body

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

render :: ECli.Action -> M.Model -> R.Route -> E.Asset LByteString
render _ _ (R.Css "stylesheet")    = E.AssetGenerated E.Other (encodeUtf8 S.styleT)
render _ m (R.Css s)               = E.AssetStatic $ E.encodeRoute m (R.Css s)
render _ m (R.Js s)                = E.AssetStatic $ E.encodeRoute m (R.Js s)
render _ m (R.BlogAsset s i)       = E.AssetStatic $ E.encodeRoute m (R.BlogAsset s i)
render _ m (R.StructuralPage path) = pageOrnotFound m $ Map.lookup path $ M.structuralPages m
render _ m (R.BlogPage slug)       = pageOrnotFound m $ Map.lookup slug $ M.blogPosts m
render _ m  R.BlogIndex            = applyDefaultLayout m $ blogIndex m
