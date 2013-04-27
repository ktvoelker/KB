
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
  textInputLike ? userSelect selectText
  header ? do
    position relative
    zIndex 60
    headerStyle
  "#app" ? do
    position relative
    zIndex 40
    width (px totalWidth)
    sym2 margin 0 auto
    sym padding 0
  "#notes" ? notes
 
