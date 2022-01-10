{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, BlockArguments #-}

-- |

module Render where

import Models
import System.FilePath ((</>))
import qualified Routes as R
import qualified Layouts as L
import qualified Style as S
import qualified Ema as E
import qualified Ema.CLI as ECli
import Data.Map (lookup, assocs)
import Data.Time (formatTime)
import Locale (ptTimeLocale)
import Lucid

data SimplePage = SimplePage { simpleTitle :: Text, simpleBody :: Html () }

instance HtmlPage SimplePage where
  title = simpleTitle
  body  = simpleBody

applyDefaultLayout :: HtmlPage a => Model -> a -> E.Asset LByteString
applyDefaultLayout m =
  E.AssetGenerated E.Html
  . renderBS
  . L.primary m (L.head m)
  . body

pageOrnotFound :: HtmlPage a => Model -> Maybe a -> E.Asset LByteString
pageOrnotFound m (Just page) = applyDefaultLayout m page
pageOrnotFound m Nothing     = applyDefaultLayout m $
  StructuralPage "404" "Esta página não existe :("

blogIndex :: Model -> SimplePage
blogIndex m = SimplePage "Blog" do
  h1_ "Blog"
  aside_ "Páginas mais curtas e mais frequentes."
  forM_ (reverse $ Data.Map.assocs $ blogPosts m) \(s,p) -> do
    h2_ $ a_ [href_ $ E.routeUrl m (R.BlogPage s)] (toHtmlRaw $ postTitle p)
    aside_ $ toHtml $ formatTime ptTimeLocale "%A, %e de %B de %Y" $ date p
    toHtmlRaw $ postBody p
    hr_ []

roamIndex :: Model -> SimplePage
roamIndex m = SimplePage "Zettelkasten" do
  h1_ "Zettelkasten"
  aside_ "My knowledge vault."
  forM_ (reverse $ Data.Map.assocs $ roamPosts m) \(uuid,p) -> do
    h2_ $ a_ [href_ $ E.routeUrl m (R.RoamPage uuid)] (toHtmlRaw $ postTitle p)
    aside_ $ toHtml $ formatTime ptTimeLocale "%A, %e de %B de %Y" $ date p

render :: ECli.Action -> Model -> R.Route -> E.Asset LByteString
render _ _ (R.Css "stylesheet")    = E.AssetGenerated E.Other (encodeUtf8 S.styleT)
render _ m (R.Css s)               = E.AssetStatic $ E.encodeRoute m (R.Css s)
render _ m (R.Js s)                = E.AssetStatic $ E.encodeRoute m (R.Js s)
render _ _ (R.BlogAsset s i)       = E.AssetStatic $ mountPoint Blog
                                     </> toString (E.unSlug s)
                                     </> toString (E.unSlug i)
render _ m (R.StructuralPage l p)  = pageOrnotFound m $ lookup l =<< lookup p (structuralPages m)
render _ m  R.RoamEntryPoint       = applyDefaultLayout m $ roamIndex m
render _ m (R.RoamPage uuid)       = pageOrnotFound m $
                                     (,lookup uuid $ roamDatabase m)
                                     <$> lookup uuid (roamPosts m)
render _ m (R.BlogPage slug)       = pageOrnotFound m $ lookup slug $ blogPosts m
render _ m  R.BlogIndex            = applyDefaultLayout m $ blogIndex m
render _ m _ = applyDefaultLayout m $ StructuralPage "Oh no!" "Not implemented."
