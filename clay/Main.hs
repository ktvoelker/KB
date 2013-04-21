
module Main where

import Clay hiding (map)

import qualified Colors

noBorder :: Css
noBorder = border none 0 black

noOutline :: Css
noOutline = outline none 0 black

raw :: Css
raw = do
  noBorder
  noOutline
  sym margin 0
  sym padding 0

union :: [Selector] -> (Selector -> a -> Css) -> a -> Css
union ss f x = mapM_ (flip f x) ss

main :: IO ()
main = putCss $ do
  "body" ? userSelect none
  union ["input", "textarea"] (?) $ userSelect selectText
  "#curtain" ? do
    position relative
    width (px 1000)
    sym2 margin 0 auto
  "#app-title" ? do
    raw
    position absolute
    left 0
    top 0
    height (px 100)
    width (px 100)
    fontSize (px 75)
    fontWeight normal
    fontFamily [sansSerif] []
  "#new-note" ? do
    left 0
    top (px 100)
  union (map (# focus) [input, button, select, textarea]) (?) $ noOutline
  "#notes" ? raw
  ".note" ? do
    Colors.noteBody
    "list-style-type" -: "none"
    width (px 700)
    position relative
    sym2 margin (px 30) auto
    minHeight (px 100)
  ".title" ? do
    Colors.noteTitle
    width (px 480)
    height (px 30)
    sym margin 0
    sym padding (px 10)
    noBorder
    fontWeight normal
    fontSize (px 28)
    fontFamily ["Georgia", "Times New Roman", serif] []
  ".updated" ? do
    raw
    Colors.noteTitle
    position absolute
    right (px 100)
    top 0
    width (px 100)
    height (px 100)
    textAlign $ alignSide sideCenter
    ".num" ? do
      raw
      position absolute
      top 0
      width (px 100)
      height (px 75)
      fontSize (px 70)
    ".unit" ? do
      raw
      position absolute
      bottom 0
      width (px 100)
      height (px 25)
      fontSize (px 18)
  ".tags" ? do
    width (px 500)
    height (px 50)
    sym margin 0
    sym padding 0
  ".body" ? raw
  union ["#new-note", ".edit-note"] (?) $ do
    Colors.button
    width (px 100)
  ".save-note" ? do
    Colors.buttonAlert
    width (px 200)
  union [".edit-note", ".save-note"] (?) $ do
    top 0
    right 0
  union ["#new-note", ".edit-note", ".save-note"] (?) $ do
    position absolute
    height (px 100)
    noBorder
 
