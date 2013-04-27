
module Note where

import Clay hiding (div)

import qualified Colors
import Config
import Util

sideColumnWidth :: Integer
sideColumnWidth = 100

columnGap :: Integer
columnGap = 20

sideMargin :: Integer
sideMargin = 40

notePadding :: Integer
notePadding = 20

contentWidth :: Integer
contentWidth = totalWidth - 2 * sideMargin

mainColumnWidth :: Integer
mainColumnWidth = contentWidth - sideColumnWidth - columnGap

buttonPadding :: Integer
buttonPadding = 40

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
    sym padding (px notePadding)
    minHeight (px minNoteHeight)
    -- TODO use Colors.midMain
    "box-shadow" -: "10px 10px 10px 0 #777777"
  ".note" # firstChild ? do
    paddingTop (px $ 20 + headerShadowRadius)
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
  button ? do
    raw
    Colors.noteButton
    float floatRight
    clear clearRight
    textAlign (alignSide sideRight)
    height (px $ minNoteHeight `div` 3)
    fontSize (px $ minNoteHeight `div` 5)
    marginTop (px 15)
    sym2 padding (px 5) (px buttonPadding)
    position relative
    left (px buttonPadding)
  button # hover ? do
    Colors.noteButtonHover
    -- TODO use Colors.midMain
    "box-shadow" -: "10px 10px 10px 0 #777777"

