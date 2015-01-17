{-# LANGUAGE OverloadedStrings #-}

import Lucid.Svg
import Data.Monoid

svg :: Svg () -> Svg ()
svg content = do
  doctype_
  with (svg11_ content) [width_ "325" , height_ "325"]

contents :: Svg ()
contents = do
  path_ (
    [ d_ (mA "10" "80" <> qA "52.5" "10" "95" "80" <> tA "180" "80" <> z)
    , stroke_ "blue"
    , fill_ "orange"
    ])

main :: IO ()
main = do
  print $ svg contents
