{-# LANGUAGE OverloadedStrings #-}

import Lucid.Svg
import Data.Monoid
import Data.Text.Lazy as T

svg :: Element -> Element
svg content =
      doctype_
   <> with (svg11_ content) [Width <<- "325", Height <<- "325"]

contents :: Element
contents =
  path_
    [ D <<- (mA 10 80 <> qA 52.5 10 95 80 <> tA 180 80 <> z)
    , Stroke <<- "blue"
    , Fill <<- "orange"
    ] nil

main :: IO ()
main = do
  print $ svg contents
