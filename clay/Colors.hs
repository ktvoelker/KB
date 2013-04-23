
module Colors where

import Clay

{--
 - From http://colorschemedesigner.com/
 - Triad mode: hue 203 degrees, angle 30 degrees
 --}

colors :: (Color, Color) -> Css
colors (bg, fg) = backgroundColor bg >> color fg

{-
darkMain, midMain, lightMain,
  darkAccent, midAccent, lightAccent,
  darkAlert, midAlert, lightAlert :: Color

darkMain    = "#006B53"
midMain     = "#00A480"
lightMain   = "#5ED2B8"
darkAccent  = "#A66800"
midAccent   = "#FF9F00"
lightAccent = "#FFCA73"
darkAlert   = "#9D0019"
midAlert    = "#F10026"
lightAlert  = "#F87085"
-}

darkMain :: Color
darkMain = "#282828"

midMain :: Color
midMain = "#DDDDDD"

lightMain :: Color
lightMain = "#E8E8E8"

noteTitle = colors (midMain, darkMain)
noteBody = colors (white, darkMain)
button = colors (white, darkMain)
buttonAlert = colors (white, lightMain)
leftColumn = colors (white, darkMain)
rightColumn = colors (white, darkMain)

