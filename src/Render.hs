{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, BlockArguments #-}

-- |

module Render where
import Data.Some (Some)
import Models
import RoamGraph
import qualified Routes as R
import qualified Layouts as L
import qualified Style as S
import qualified Ema as E
import qualified Ema.CLI as ECli
import Data.Map (lookup, assocs)
import Data.Time (formatTime)
import Locale (ptTimeLocale)
import Lucid
import Data.Aeson
import Path

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

roamIndex :: Model -> SimplePage
roamIndex m = SimplePage "Zettelkasten" do
  h1_ "Zettelkasten"
  aside_ "My knowledge vault."
  forM_ (reverse $ Data.Map.assocs $ roamPosts m) \(uuid,p) -> do
    h2_ $ a_ [href_ $ E.routeUrl m (R.RoamPage uuid)] (toHtmlRaw $ postTitle p)
    aside_ $ toHtml $ formatTime ptTimeLocale "%A, %e de %B de %Y" $ date p

render :: Some ECli.Action -> Model -> R.Route -> E.Asset LByteString

render _ _ R.StyleSheet  =
  E.AssetGenerated E.Other (encodeUtf8 S.styleT)

render _ m (R.StructuralPage l p)  =
  pageOrnotFound m $ lookup l =<< lookup p (structuralPages m)

render _ m  R.RoamEntryPoint =
  applyDefaultLayout m $ roamIndex m
render _ m  R.RoamGraphJSON =
  E.AssetGenerated E.Other $ encode $ buildRoamGraph m
render _ m (R.RoamPage uuid) =
  pageOrnotFound m $
  (,lookup uuid $ roamDatabase m)
  <$> lookup uuid (roamPosts m)

render _ _ (R.StaticAsset source path)  =
  E.AssetStatic $ mountPoint source /> pathToUrl path
