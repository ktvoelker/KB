
module Config where

import Clay

sideColumnWidth, mainColumnWidth, columnGap :: Integer

sideColumnWidth = 100
mainColumnWidth = 600
columnGap = 50

serifFamily :: Css
serifFamily = fontFamily ["Georgia", "Times New Roman", serif] []

normalPad :: Integer
normalPad = 10

totalWidth :: Integer
totalWidth = mainColumnWidth + (sideColumnWidth + columnGap) * 2

minNoteHeight :: Integer
minNoteHeight = 100

