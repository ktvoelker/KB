
module Util where

import Clay

noBorder :: Css
noBorder = border none 0 black

noOutline :: Css
noOutline = outline none 0 black

raw :: Css
raw = do
  noBorder
  noOutline
  sym margin 0
  sym padding 0

union :: [Selector] -> (Selector -> a -> Css) -> a -> Css
union ss f x = mapM_ (flip f x) ss

