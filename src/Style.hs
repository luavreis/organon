{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Style where

import Clay
import Prelude hiding ((**),empty,rem,div)
import Text.Pandoc.Highlighting (tango, styleToCss)
import qualified Clay.Media as CM

backgroundWhite :: Color
backgroundWhite = "#fffdf8"

darkTduration :: Double
darkTduration = 0.2

sAttr :: (Semigroup a1, IsString a1, Show a2) => a1 -> a2 -> a1
sAttr n x = n <> " " <> show x <> "s"

r_ :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Css
r_ f x = f x x x x

r__ :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Size a -> Css
r__ f x y = f x y x y

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
    r__ margin (em 6) (em 1)
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
    content (stringContent "≔")
    fontFamily [] [monospace]
    paddingLeft (em 0.3)
    paddingRight (em 0.2)

  dd ? do
    marginLeft (px 20)
    marginBottom (em 0.4)

sections :: Css
sections = do
  queryOnly CM.screen [CM.minWidth (px 600)] do
    section ? do
      borderLeft solid (px 2) (darken 0.05 backgroundWhite)
      paddingLeft (px 15)

images :: Css
images = do
  img ? do
    marginLeft auto
    marginRight auto
    maxWidth (pct 100)

  p |> img ? do
    verticalAlign vAlignBottom
    r__ margin 0 (px 3)

  p ** img # onlyChild ? do
    display block
    marginLeft auto
    marginRight auto

  figure ** img ?
    display block

quotes :: Css
quotes = do
  blockquote ? do
    fontStyle italic
    r__ margin (em 1) (em 3)

lists :: Css
lists = do
  li # "::marker" ? do
    fontWeight (weight 500)

  ul <> ol ? do
    r__ margin (px 8) (px 0)
    lineHeight (em 1.3)

  ul ? paddingLeft (px 5)

  ul ** li ? do
    -- listStyleType none
    -- paddingLeft (px 27)
    marginLeft (px 20)
    position relative

  -- ul ** li # before ? do
    -- content (stringContent "~")
    -- left (px 8)
    -- position absolute

  (ol <> ul) ** li ? do
    marginBottom (em 0.4)

headings :: Css
headings = do
  h1 ? do
    fontSize (px 40)
    fontWeight normal
    -- fontStyle italic
    letterSpacing (px (-0.2))

  h2 ? do
    fontSize (px 32)
    marginTop (px 20)
    fontWeight (weight 500)
    a # link <> a # visited ?
      color black
    a # hover ?
      color (grayish 90)

  h3 <> h4 <> h5 <> h6 ? do
    marginTop (px 10)
    fontWeight (weight 500)

  h3 ? fontSize (px 28)
  h4 <> h5 <> h6 ? fontSize (px 26)
  -- (h2 <> h3 <> h4 <> h5 <> h6) # before ? do
  --   fontSize (pct 45)
  --   verticalAlign middle
  --   letterSpacing (px 3)

  -- h2 # before ? content (stringContent "▲")
  -- h3 # before ? content (stringContent "■■")
  -- h4 # before ? content (stringContent "⬟⬟⬟")
  -- h5 # before ? content (stringContent "⬢⬢⬢⬢")
  -- h6 # before ? content (stringContent "⚫⚫⚫⚫⚫")

fonts :: Css
fonts = do
  -- fontFace do
  --   fontFamily ["Crimson Pro"] []
  --   fontFaceSrc [FontFaceSrcUrl "/assets/fonts/CrimsonPro.ttf" $ Just TrueType ]
  -- fontFace do
  --   fontFamily ["Crimson Pro"] []
  --   fontStyle italic
  --   fontFaceSrc [FontFaceSrcUrl "/assets/fonts/CrimsonPro-Italic.ttf" $ Just TrueType ]
  fontFace do
    fontFamily ["twemoji mozilla"] []
    fontFaceSrc [FontFaceSrcUrl "https://xem.github.io/unicode13/Twemoji.ttf" $ Just TrueType ]
  fontFace do
    fontFamily ["victor mono"] []
    fontFaceSrc [FontFaceSrcUrl "/assets/fonts/VictorMono-Medium.woff2" $ Just WOFF2]

emojis :: Css
emojis = do
  ".emoji" ? do
    fontFamily ["twemoji mozilla", "noto color emoji"] []
    display inlineBlock

codes :: Css
codes = do
  code ? do
    fontFamily ["victor mono"] [monospace]
    fontSize (pct 83)
    -- letterSpacing (px 0.3)

  div # ".sourceCode" ? do
    r__ padding (em 0.3) (em 0.5)
    r_ borderRadius (em 1)

tables :: Css
tables = do
  table ? do
    width (pct 80)
    marginLeft auto
    marginRight auto

pageHeader :: Css
pageHeader = do
  header ? do
    marginBottom (px 28)

main :: Css
main = do
  star ? do
    r_ margin (px 0)

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
    background         backgroundWhite
    color              ("#333" :: Color)
    fontSize (px 19.5)
    fontFamily         ["Crimson Pro", "times"]  [serif]
    fontWeight         (weight 300)
    textRendering      optimizeLegibility
    "font-kerning"     -: "normal"
    "scroll-behaviour" -: "smooth"

  a ? do
    textDecoration none
    fontWeight (weight 400)
  a # link <> a # visited <> a # hover ? do
    -- background (setA 0.6 moccasin)
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
    position relative
    width (pct 90)
    maxWidth (px 800)
    minHeight (vh 80)
    paddingLeft (pct 5)
    paddingRight (pct 5)
    top (px 40)
    r__ margin (px 0) auto

    lists
    tables
    headings
    quotes
    codes
    images
    definitionList

    p ? do
      marginTop (px 10)
      marginBottom (px 10)
      lineHeight (em 1.4)

    a # link <> a # visited ? do
      textDecoration underline

    strong ? fontWeight (weight 500)

    figcaption ? textAlign center

    hr ? do
      borderColor "#fff"
      width (pct 70)
      r__ margin (em 4) auto

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

style :: LText
style = renderWith compact []
         (do main
             fonts
             emojis
             sections
             pageHeader
             foot
             katex
             citationsCss
             darkStyle
         )
         <> toLText (styleToCss tango)
