
module Main where

import Clay hiding (map, div)

import qualified Colors
import Config
import Header
import Note
import Util

main :: IO ()
main = putCss $ do
  "body" ? do
    raw
    userSelect none
    Colors.background
  union ["input", "textarea"] (?) $ userSelect selectText
  union (map (# focus) [input, button, select, textarea]) (?) $ noOutline
  header ? do
    position relative
    zIndex 60
    headerStyle
  "#app" ? do
    position relative
    zIndex 40
    Colors.app
    width (px totalWidth)
    sym2 margin 0 auto
    paddingTop (px headerShadowRadius)
  "#notes" ? notes
 
