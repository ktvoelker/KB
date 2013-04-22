
module Main where

import Clay hiding (map, div)

import qualified Colors
import Config
import Util

totalWidth :: Integer
totalWidth = mainColumnWidth + (sideColumnWidth + columnGap) * 2

minNoteHeight :: Integer
minNoteHeight = 100

squareButtonSize :: Css
squareButtonSize = do
  height (px minNoteHeight)
  width (px minNoteHeight)

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
    position absolute
    left 0
    top 0
    squareButtonSize
    fontSize (px $ minNoteHeight * 3 `div` 4)
    fontWeight normal
    fontFamily [sansSerif] []
  "#new-note" ? do
    left 0
    top (px minNoteHeight)
  union (map (# focus) [input, button, select, textarea]) (?) $ noOutline
  "#notes" ? raw
  ".note" ? do
    Colors.noteBody
    "list-style-type" -: "none"
    width (px mainColumnWidth)
    position relative
    sym2 margin (px 30) auto
    minHeight (px minNoteHeight)
  ".title" ? do
    Colors.noteTitle
    width (px $ mainColumnWidth - (normalPad * 2) - minNoteHeight)
    height (px 30)
    sym margin 0
    sym padding (px normalPad)
    noBorder
    fontWeight normal
    fontSize (px 28)
    serifFamily
  ".updated" ? do
    raw
    Colors.rightColumn
    position absolute
    top 0
    left (px mainColumnWidth)
    width (px sideColumnWidth)
    height (px minNoteHeight)
    sym2 padding (px normalPad) (px $ normalPad * 2)
  ".tags" ? do
    raw
    width (px $ mainColumnWidth - minNoteHeight)
    height (px 50)
  ".body" ? do
    raw
    Colors.noteBody
    fontSize (pt 13)
    serifFamily
    width (px $ mainColumnWidth - normalPad * 2)
    sym padding (px normalPad)
  union ["#new-note", ".edit-note"] (?) $ do
    Colors.button
  ".save-note" ? do
    Colors.buttonAlert
  union [".edit-note", ".save-note"] (?) $ do
    top 0
    right 0
  union ["#new-note", ".edit-note", ".save-note"] (?) $ do
    raw
    position absolute
    squareButtonSize
    fontSize (px $ minNoteHeight `div` 5)
 
