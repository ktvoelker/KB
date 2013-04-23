
module Note where

import Clay hiding (div)

import qualified Colors
import Config
import Util

contentWidth :: Integer
contentWidth = mainColumnWidth - sideColumnWidth - columnGap

notes :: Css
notes = do
  raw
  width (px mainColumnWidth)
  sym2 margin 0 auto
  ".note" ? do
    Colors.noteBody
    "list-style-type" -: "none"
    display block
    marginBottom (px 30)
    minHeight (px minNoteHeight)
  ".title" ? do
    raw
    Colors.noteTitle
    fontWeight normal
    fontSize (px 28)
    serifFamily
    width (px contentWidth)
  ".tags" ? do
    raw
    width (px contentWidth)
  ".body" ? do
    raw
    Colors.noteBody
    fontSize (pt 13)
    serifFamily
    width (px contentWidth)
  ".updated" ? do
    raw
    Colors.rightColumn
    width (px sideColumnWidth)
    float floatRight
  ".edit-note" ? Colors.button
  ".save-note" ? Colors.buttonAlert
  union [".edit-note", ".save-note"] (?) $ do
    raw
    float floatRight
    clear clearRight
    height (px $ minNoteHeight `div` 3)
    width (px sideColumnWidth)
    fontSize (px $ minNoteHeight `div` 5)

