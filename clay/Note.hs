
module Note where

import Clay hiding (div)

import qualified Colors
import Config
import Util

sideColumnWidth :: Integer
sideColumnWidth = 80

columnGap :: Integer
columnGap = 20

sideMargin :: Integer
sideMargin = 40

contentWidth :: Integer
contentWidth = totalWidth - 2 * sideMargin

mainColumnWidth :: Integer
mainColumnWidth = contentWidth - sideColumnWidth - columnGap

notes :: Css
notes = do
  raw
  width (px contentWidth)
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
    width (px mainColumnWidth)
  ".tags" ? do
    raw
    width (px mainColumnWidth)
  ".body" ? do
    raw
    Colors.noteBody
    fontSize (pt 13)
    serifFamily
    width (px mainColumnWidth)
  ".updated" ? do
    raw
    Colors.rightColumn
    width (px sideColumnWidth)
    float floatRight
    textAlign (alignSide sideRight)
  ".edit-note" ? Colors.button
  ".save-note" ? Colors.buttonAlert
  union [".edit-note", ".save-note"] (?) $ do
    raw
    float floatRight
    clear clearRight
    textAlign (alignSide sideRight)
    height (px $ minNoteHeight `div` 3)
    width (px sideColumnWidth)
    fontSize (px $ minNoteHeight `div` 5)

