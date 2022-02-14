{-# LANGUAGE BlockArguments, ExtendedDefaultRules, OverloadedStrings #-}

module Layouts where

import Lucid
import Lucid.Base (makeAttribute)
import Models
import Routes ()
import Data.Map ((!?))
import Data.Text as T

twemoji :: Text -> Html ()
twemoji e = span_ [class_ "emoji"] $ toHtmlRaw e

-- Lucid definitions
obdata_ :: Text -> Attribute
obdata_ = makeAttribute "data"


renderLayout :: Model -> String -> Html ()
renderLayout model key = maybe mempty toHtmlRaw (layouts model !? key)

head :: Model -> Html ()
head m = head_ do
  base_ [href_ "/"] -- Importante para a validade dos links em trechos
                    -- "cross-página"
  meta_ [charset_ "utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"]
  title_ (toHtml $ siteName m) -- + título do post TODO
  css "stylesheet"
  css "latex"
  js "extras"
  js "darktoggle"
  -- flip (maybe mempty) (encodeSlug <$> wandererLocation m) $
  --   \ loc ->
  --     script_ [type_ "text/javascript"] $
  --       "if (typeof wanderer === 'undefined') { var wanderer = '' }"
  --       <> "var url = \"/zettelkasten/" <> loc <>"\";"
  --       <> "if (window.location.pathname != url && wanderer != url) {\
  --          \wanderer = url;\
  --          \var link = document.createElement('a');\
  --          \link.href = url;\
  --          \document.body.appendChild(link);\
  --          \link.click();}"
  link_ [rel_ "dns-prefetch", href_ "//fonts.googleapis.com"]
  link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ ""]
  link_ [stylesheet, href_ "https://fonts.googleapis.com/css2?family=Crimson+Pro:ital,wght@0,200;0,300;0,400;0,500;1,200;1,300;1,500&display=swap"]
  renderLayout m "katex"
  where
    js :: Text -> Html ()
    js s = script_ [src_ $ "/assets/js/" <> s <> ".js", defer_ "", type_ "text/javascript"] T.empty
    css s = link_ [stylesheet, href_ $ "/assets/css/" <> s <> ".css", type_ "text/css"]
    stylesheet = rel_ "stylesheet"

foot :: Html ()
foot =
  div_ [class_ "page-foot"] do
    "Site em construção... "
    twemoji "🏗"
    br_ []
    button_ [id_ "extra1"] do
      "Katamari! "
      twemoji "🏐"
    " ~ "
    button_ [id_ "extra2"] do
      "Boids "
      twemoji "🐦"

primary :: Model -> Html () -> Html () -> Html ()
primary _ docHead content =
  doctypehtml_ $
  html_ [lang_ "pt-br"] do
    docHead
    body_ do
      canvas_ [style_ "position:fixed; top:0; left:0; z-index:-1;", id_ "fundo"] ""
      div_ [id_ "main"] do
        header_ do
          a_ [href_ "/", style_ "background: none;"] do
            twemoji "🏡" <> " início"
        content
        foot
