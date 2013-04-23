
module Main where

import Clay hiding (map, div)

import qualified Colors
import Config
import Note
import Util

main :: IO ()
main = putCss $ do
  "body" ? userSelect none
  union ["input", "textarea"] (?) $ userSelect selectText
  "#curtain" ? do
    position relative
    width (px totalWidth)
    sym2 margin 0 auto
  "#app-title" ? do
    raw
    Colors.leftColumn
    position absolute
    left 0
    top 0
    width (px minNoteHeight)
    height (px minNoteHeight)
    fontSize (px $ minNoteHeight * 3 `div` 4)
    fontWeight normal
    fontFamily [sansSerif] []
  union (map (# focus) [input, button, select, textarea]) (?) $ noOutline
  notes
 
