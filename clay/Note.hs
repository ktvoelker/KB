
module Note where

import Clay hiding (div)

import qualified Colors
import Config
import Util

notes :: Css
notes = do
  "#notes" ? do
    raw
  ".note" ? do
    Colors.noteBody
    "list-style-type" -: "none"
    width (px mainColumnWidth)
    position relative
    sym2 margin (px 30) auto
    minHeight (px minNoteHeight)
  ".title" ? do
    Colors.noteTitle
    width (px $ mainColumnWidth - (normalPad * 2))
    height (px 30)
    sym margin 0
    sym padding (px normalPad)
    noBorder
    fontWeight normal
    fontSize (px 28)
    serifFamily
  ".tags" ? do
    raw
    width (px mainColumnWidth)
    height (px 50)
  ".body" ? do
    raw
    Colors.noteBody
    fontSize (pt 13)
    serifFamily
    width (px $ mainColumnWidth - normalPad * 2)
    sym padding (px normalPad)
  ".updated" ? do
    raw
    Colors.rightColumn
    position absolute
    top 0
    left (px mainColumnWidth)
    width (px sideColumnWidth)
    height (px $ minNoteHeight `div` 3)
    sym2 padding (px normalPad) (px $ normalPad * 2)
  "#new-note" ? do
    left 0
    top (px minNoteHeight)
    margin (px normalPad) (px $ normalPad * 2) (px normalPad) 0
  union ["#new-note", ".edit-note"] (?) $ do
    Colors.button
  ".save-note" ? do
    Colors.buttonAlert
  union [".edit-note", ".save-note"] (?) $ do
    top (px $ minNoteHeight `div` 3)
    left (px mainColumnWidth)
    sym2 margin (px normalPad) (px $ normalPad * 2)
    margin (px normalPad) 0 (px normalPad) (px $ normalPad * 2)
  union ["#new-note", ".edit-note", ".save-note"] (?) $ do
    raw
    position absolute
    height (px $ minNoteHeight `div` 3)
    width (px sideColumnWidth)
    fontSize (px $ minNoteHeight `div` 5)

