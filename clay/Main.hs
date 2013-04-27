
module Main where

import Clay hiding (map, div)
import Data.Monoid

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
  "input" <> "textarea" ? userSelect selectText
  inputLike # focus ? noOutline
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
 
