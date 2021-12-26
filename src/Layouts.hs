{-# LANGUAGE BlockArguments, ExtendedDefaultRules, OverloadedStrings #-}

module Layouts where

import Lucid
import Lucid.Base (makeAttribute, ToHtml (toHtmlRaw))
import Ema (Ema(encodeRoute), Asset)
import Relude.Extra (lookupDefault)
import Routes
import Models
import EmaInstance
import Data.Map ((!))
import qualified Ema as E

twemoji :: Text -> Html ()
twemoji t = i_ [class_ $ "twa twa-" <> t] ""

-- Lucid definitions
obdata_ :: Text -> Attribute
obdata_ = makeAttribute "data"

stylesheet :: Attribute
stylesheet = rel_ "stylesheet"

renderRawAsset :: Model -> RawAssetId -> Html ()
renderRawAsset model id = toHtmlRaw $ rawAssets model ! id

head :: Model -> Html ()
head m = head_ do
  base_ [href_ "/"]
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  title_ (toHtml $ name m) -- + título do post TODO
  css "twemoji"
  css "stylesheet"
  css "latex"
  js "extras"
  js "darktoggle"
  link_ [rel_ "dns-prefetch", href_ "//fonts.googleapis.com"]
  link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ ""]
  link_ [stylesheet, href_ "https://fonts.googleapis.com/css2?family=Crimson+Pro:ital,wght@0,300;0,400;0,500;1,300;1,500&display=swap"]
  link_ [stylesheet, href_ "https://fonts.googleapis.com/css2?family=Patrick+Hand&display=swap"]
  renderRawAsset m (RawHtmlId "katex")
  where
    js :: Text -> Html ()
    js s = script_ [src_ $ "/assets/js/" <> s <> ".js", defer_ "", type_ "text/javascript"] ""
    css s = link_ [stylesheet, href_ $ "/assets/css/" <> s <> ".css", type_ "text/css"]

foot :: Html ()
foot =
  div_ [class_ "page-foot"] do
    "Site em construção..."
    twemoji "building-construction"
    br_ []
    button_ [id_ "extra1"] do
      "Katamari!"
      twemoji "volleyball"
    " ~ "
    button_ [id_ "extra2"] do
      "Boids"
      twemoji "bird"


navUl :: Model -> Html ()
navUl m = ul_ $ forM_ topLevel \
  t -> li_ $ a_ [href_ $ toText $ E.routeUrl m $ snd t] $ toHtml $ fst t

primary :: Model -> Html () -> Html () -> Html ()
primary model head content =
  doctypehtml_ $
  html_ [lang_ "pt-br"] do
    head
    body_ do
      canvas_ [style_ "position:fixed; top:0; left:0; z-index:-1;", id_ "fundo"] ""
      header_ do
        blogname
        darkbutton
        nav_ (navUl model <> object_ [id_ "menu-icon", obdata_ "/assets/icons/hamburger.svg", type_ "image/svg+xml"] "Menu")
      div_ [id_ "main"] (content <> foot)
      div_ [id_ "sidebar"] (blogname <> navUl model <> darkbutton)
  where
    blogname = h2_ [class_ "blog-name"] (a_ [href_ "/"] (toHtml (name model)))
    darkbutton = div_ [class_ "dark-mode"] $
                 button_ [onclick_ "darkToggle()"] $
                   object_ [obdata_ "/assets/icons/darkmode.svg", type_ "image/svg+xml"] "Dark"
