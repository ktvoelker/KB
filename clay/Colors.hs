
module Colors where

import Clay hiding (header)

colors :: (Color, Color) -> Css
colors (bg, fg) = backgroundColor bg >> color fg

darkMain :: Color
darkMain = "#282828"

midDarkMain :: Color
midDarkMain = "#555555"

midMain :: Color
midMain = "#777777"

midLightMain :: Color
midLightMain = "#DDDDDD"

lightMain :: Color
lightMain = "#E8E8E8"

noteTitle = colors (white, darkMain)
noteBody = colors (white, darkMain)
noteButton = colors (transparent, darkMain)
noteButtonHover = colors (darkMain, white)
leftColumn = colors (white, darkMain)
rightColumn = colors (white, darkMain)
header = colors (midDarkMain, white)
headerButton = header
headerButtonHover = colors (white, midDarkMain)
background = colors (lightMain, darkMain)

