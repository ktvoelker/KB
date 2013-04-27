
module Header where

import Clay

import qualified Colors
import qualified Note
import Config
import Util

headerFont :: Css
headerFont = do
  fontSize (px 30)
  fontWeight normal
  "font-family" -: "sans-serif"
  "line-height" -: "1"

contentWidth :: Integer
contentWidth = totalWidth - 2 * (Note.sideMargin + Note.notePadding)

headerStyle :: Css
headerStyle = do
  Colors.header
  padding (px 10) (px 10) 0 (px 10)
  margin 0 0 (px 10) 0
  height (px 30)
  -- TODO should use headerShadowRadius
  "box-shadow" -: "0 0 10px 10px #555555"
  "#header-content" ? do
    width (px contentWidth)
    sym2 margin 0 auto
  "#app-title" ? do
    raw
    headerFont
    float floatLeft
  "#menu" ? do
    raw
    float floatRight
    height (pct 100)
  "#menu" |> li ? do
    raw
    "list-style-type" -: "none"
    headerFont
    float floatLeft
    height (pct 100)
  "#new-note" ? do
    raw
    Colors.header
    fontSize (px 20)
    "line-height" -: "20px"
    "vertical-align" -: "baseline"

