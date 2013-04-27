
module Colors where

import Clay

colors :: (Color, Color) -> Css
colors (bg, fg) = backgroundColor bg >> color fg

darkMain :: Color
darkMain = "#282828"

midDarkMain :: Color
midDarkMain = "#555555"

midMain :: Color
midMain = "#DDDDDD"

lightMain :: Color
lightMain = "#E8E8E8"

noteTitle = colors (white, darkMain)
noteBody = colors (white, darkMain)
button = colors (white, darkMain)
buttonAlert = colors (darkMain, lightMain)
leftColumn = colors (white, darkMain)
rightColumn = colors (white, darkMain)
header = colors (midDarkMain, white)
background = colors (lightMain, darkMain)

