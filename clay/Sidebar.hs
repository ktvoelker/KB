
module Sidebar where

import Clay hiding (div)

import qualified Colors
import Config
import Util

sidebar :: Css
sidebar = do
  float floatLeft
  width (px sideColumnWidth)
  "#app-title" ? do
    raw
    Colors.leftColumn
    width (px minNoteHeight)
    height (px minNoteHeight)
    fontSize (px $ minNoteHeight * 3 `div` 4)
    fontWeight normal
    fontFamily [sansSerif] []
  "#new-note" ? do
    raw
    height (px $ minNoteHeight `div` 3)
    fontSize (px $ minNoteHeight `div` 5)
    Colors.button

