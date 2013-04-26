
module Config where

import Clay

totalWidth :: Integer
totalWidth = 700

serifFamily :: Css
-- TODO does serif produce the wrong output?
serifFamily = fontFamily ["Georgia", "Times New Roman", serif] []

normalPad :: Integer
normalPad = 10

minNoteHeight :: Integer
minNoteHeight = 100

headerShadowRadius :: Integer
headerShadowRadius = 10

