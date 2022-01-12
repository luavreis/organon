{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Style where

import Clay
import Prelude hiding ((**),empty)
import qualified Clay.Media as M

darkTduration :: Double
darkTduration = 0.2

sAttr :: (Semigroup a1, IsString a1, Show a2) => a1 -> a2 -> a1
sAttr n x = n <> " " <> show x <> "s"

margin_ :: Size a -> Css
margin_ x = margin x x x x

margin__ :: Size a -> Size a -> Css
margin__ x y = margin x y x y

padding_ :: Size a -> Css
padding_ x = padding x x x x

citationsCss :: Css
citationsCss = do
  ".csl-left-margin" ? display inline
  ".csl-right-inline" ? display inline

foot :: Css
foot = do
  ".page-foot" ? do
    fontSize (pct 70)
    fontFamily ["Arial"] [sansSerif]
    color "#a6a2a0"
    textAlign center
    margin__ (em 6) (em 1)
    lineHeight (em 2)
  button ? do
    background (none :: Color)
    cursor pointer
    color "#a6a2a0"
    borderStyle none
    textDecoration underline

katex :: Css
katex = do
  span # ".math" ? do
    letterSpacing (px (-10))
    visibility hidden

  span # ".math" ** ".katex" ? do
    letterSpacing (px 0)
    visibility visible
    overflowX auto
    overflowY hidden

definitionList :: Css
definitionList = do
  dt ? do
    float floatLeft
    clear clearLeft
    marginRight (px 5)
    fontWeight (weight 500)
    background beige
    color (setA 0.8 black)

  dl ? do
    marginLeft (em 1)
    marginTop (em 0.5)
    marginBottom (em 0.5)

  dt # after ? do
    content (stringContent " ::")

  dd ? do
    marginLeft (px 20)
    marginBottom (em 0.4)

images :: Css
images = do
  img ? do
    marginLeft auto
    marginRight auto
    maxWidth (pct 70)

  p ** img # onlyChild ? do
    display block

  figure ** img ?
    display block

lists :: Css
lists = do
  ul <> ol ? do
    margin__ (px 8) (px 0)

  ul ? paddingLeft (px 5)

  ul ** li ? do
    listStyleType none
    paddingLeft (px 27)
    position relative

  ul ** li # before ? do
    content (stringContent "~")
    left (px 8)
    position absolute

  (ol <> ul) ** li ? do
    marginBottom (em 0.4)

headings :: Css
headings = do
  h1 ? do
    fontSize (px 36)
    fontWeight normal
    -- fontStyle italic
    letterSpacing (px (-0.2))

  h2 ? do
    marginTop (px 20)
    fontWeight (weight 500)
    a # link <> a # visited ?
      color black
    a # hover ?
      color (grayish 90)

  h3 ? do
    marginTop (px 10)
    fontSize (px 22)
    fontWeight (weight 500)

  h4 ? do
    marginTop (px 10)
    fontSize (px 20)
    fontWeight (weight 500)

pageHeader :: Css
pageHeader = do
  header ? do
    marginBottom (px 8)

mainStyle :: Css
mainStyle = do
  star ? do
    margin_  (px 0)

  "#fundo" ? do
    width (pct 100)
    height (pct 100)

  html ?
    height (pct 100)

  let transP sel prop m = sel ? (do transitionProperty prop; transitionDelay $ sec (m * darkTduration))

  transP hr "background-color" 1
  transP (body <> pre) "background" 1
  transP (img # ("src" @= "svg")) "filter" 2

  ".center" ? do
    textAlign center

  body ? do
    background         ("#fffdf8" :: Color)
    color              "#111"
    fontFamily         ["Crimson Pro", "times"]  [serif]
    fontWeight         (weight 300)
    textRendering      optimizeLegibility
    "font-kerning"     -: "normal"
    "scroll-behaviour" -: "smooth"

  a ? do
    textDecoration none
    fontWeight (weight 400)
  a # link <> a # visited <> a # hover ? do
    background (setA 0.8 moccasin)
    color (setA 0.8 black)

  ".block" ? do
    display inlineBlock
    maxWidth (px 340)
    marginRight (px 40)
    "vertical-align" -: "top"

  aside ? do
    fontSize (pct 85)
    color "#777"
    fontStyle italic

  "#main" ? do
    fontSize (px 18)
    position relative
    width (pct 90)
    maxWidth (px 700)
    minHeight (vh 80)
    paddingLeft (pct 5)
    paddingRight (pct 5)
    top (px 40)
    margin__ (px 0) auto

    lists

    headings

    p ? do
      marginTop (px 10)
      marginBottom (px 10)
      lineHeight (em 1.4)

    a # link <> a # visited ? do
      textDecoration underline

    strong ? fontWeight (weight 500)

    figcaption ? textAlign center

    hr ? do
      backgroundColor "#bbb"
      borderStyle none
      height (px 0.5)
      width (pct 70)
      margin__ (em 4) auto

  ".dark-mode" ? do
    button ? do
      width (px 30)
      height (px 30)
      display inline
      paddingLeft (px 1)
      paddingTop (px 1)
      borderColor transparent
      borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
      background (other "none" :: Color)
    button # hover ? background ("#ccc3" :: Color)

darkStyle :: Css
darkStyle = do
  ".dark" ? do
    body ? do
      background bgdark
      color white
    a # link <> a # visited ?
      color "#eeff9d"
    ".blog-name" ** a ?
      color "#f5e7e3"
    where
      bgdark :: Color
      bgdark = "#171716"
      -- paper :: Color
      -- paper = "#efefef"

styleT :: LText
styleT = renderWith compact [] $ do
  mainStyle
  definitionList
  pageHeader
  images
  foot
  katex
  citationsCss
  darkStyle
