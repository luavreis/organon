{-# LANGUAGE BlockArguments, OverloadedStrings #-}

module Style where

import Clay
import Prelude hiding ((**),empty)
import Clay.Media (minWidth)
import qualified Clay.Media as M

darkTduration = 0.2

sAttr n s = n <> " " <> show s <> "s"
margin_ s = margin s s s s
margin__ u v = margin u v u v
padding_ s = padding s s s s

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

sidebar :: Css
sidebar = do
  "#sidebar" ? do
    position absolute
    top (px 40)
    width (other "calc(max(150px, 80px + 10%))") -- TODO transformar em haskell
    padding (px 20) 20 0 0
    borderRight solid (px 1) "#ccc"
    textAlign end
    ul ? do
      listStyleType none
      margin (px 20) 0 20 0
      lineHeight (px 30)
      a ? do
        fontSize (px 18)
        fontWeight (weight 500)
    ".dark-mode" ? do
      position fixed
      bottom (px 50)
      left (px 50)

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

mainStyle :: Css
mainStyle = do
  star ? do
    margin_  (px 0)
    padding_ (px 0)

  "#fundo" ? do
    width (pct 100)
    height (pct 100)

  html ?
    height (pct 100)

  let transP s p m = s ? (do transitionProperty p; transitionDelay $ sec (m * darkTduration))

  transP hr "background-color" 1
  transP (body <> pre) "background" 1
  transP (img # ("src" @= "svg")) "filter" 2
  transP ("#sidebar" <> ".comment") "border-color" 1

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
  a # link <> a # visited ?
    color "#7fa31a"
  a # hover ? color "#df8427"

  aside ? do
    fontSize (pct 85)
    color "#777"
    fontStyle italic

  "#main" ? do
    fontSize (px 17)
    position relative
    width (other "calc(85% - 150px)")
    minHeight (vh 80)
    top (px 40)
    left (other "calc(100px + 15%)")

    h1 ? do
      fontSize (px 46)
      fontWeight normal
      fontStyle italic
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

    p ? do
      margin__ (px 10) 0
      lineHeight (em 1.4)

      a # link <> a # visited ? do
        textDecoration underline
        color (other "unset")

    strong ? fontWeight (weight 500)

    ul <> ol ? margin_ (px 0)

    ul ** li ? do
      lineHeight (em 1.4)
      listStyleType none

      a # link <> a # visited ? textDecoration underline

    ul ** li # before ? do
      content (stringContent "~")
      margin__ 0 (px 8)

    img ? do
      maxWidth (pct 100)
      marginLeft auto
      marginRight auto

    p ** img # onlyChild ? do
      display block

    figcaption ? textAlign center

    hr ? do
      backgroundColor "#bbb"
      borderStyle none
      height (px 0.5)
      width (pct 70)
      margin__ (em 4) auto

  ".blog-name" ** a ? do
    fontFamily ["Patrick Hand"] [cursive]
    fontSize (px 22)
    fontWeight normal
    color "#555"
    lineHeight (px 20)

  header ? do
    display none
    height (px 35)
    margin (px 10) 10 10 20
    alignItems center

    ".blog-name" ? do
      display inlineBlock
      width (pct 90)

    ".dark-mode" ? do
      marginRight (pct 8)
      marginTop (px 2)

    nav ? do
      width (pct 10)

      "#menu-icon" ? do
        width (px 20)
        display inlineBlock
        verticalAlign middle

      li ? do
        display block
        padding_ (px 5)

    nav ** ul <> nav # active ** ul ? do
          display none
          position absolute
          top (px 15)
          right (px 10)
          padding_ (px 5)
          width auto
          fontWeight (weight 400)
          background ("#fff" :: Color)
          border solid (px 1) "#444"
          borderRadius (px 8) 4 8 8
          zIndex 1

    nav # hover ** ul ? do
      display block

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

  query Clay.all [M.minWidth (other "calc(720px / 0.85)")] do
    "#main" ? width (px 600)

  query Clay.all [M.maxWidth (px 700)] do
    "#main" ? do
      width (other "unset")
      left (px 0)
      top (px 10)
      paddingLeft (pct 5)
      paddingRight (pct 5)

    "#sidebar" ? do
      display none

    header ? do
      display flex

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
      paper :: Color
      paper = "#efefef"

styleT :: LText
styleT = renderWith compact [] $ do
  mainStyle
  foot
  sidebar
  katex
  citationsCss
  darkStyle
