
module Main where

import Clay hiding (map, div)

import Config
import Note
import Sidebar
import Util

main :: IO ()
main = putCss $ do
  "body" ? userSelect none
  union ["input", "textarea"] (?) $ userSelect selectText
  union (map (# focus) [input, button, select, textarea]) (?) $ noOutline
  "#curtain" ? do
    width (px totalWidth)
    sym2 margin 0 auto
  "#sidebar" ? sidebar
  "#notes" ? notes
 
