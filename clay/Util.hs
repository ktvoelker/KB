
module Util where

import Clay hiding (map)
import Data.Monoid

noBorder :: Css
noBorder = border none 0 black

noOutline :: Css
noOutline = outline none 0 black

raw :: Css
raw = do
  noBorder
  sym margin 0
  sym padding 0

union :: [Selector] -> (Selector -> a -> Css) -> a -> Css
union ss f x = mapM_ (flip f x) ss

buttonLike :: Selector
buttonLike = mconcat1 $ button : map ((input #) . ("type" @=)) ["submit", "reset"]

inputLike :: Selector
inputLike = mconcat1 [select, button, textInputLike]

textInputLike :: Selector
textInputLike = mconcat1
  [ input
  , textarea
  , star # ("contenteditable" @= "true")
  ]

mconcat1 :: (Monoid m) => [m] -> m
mconcat1 = foldr1 (<>)

withAnyOf :: Selector -> [Refinement] -> Selector
withAnyOf sel refs = mconcat1 $ map (sel #) refs

