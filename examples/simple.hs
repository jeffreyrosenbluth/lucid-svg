{-# LANGUAGE OverloadedStrings #-}

import Lucid.Svg
import Lucid.Base

svg :: Html () -> Html ()
svg content = do
  doctype_
  with (svg11_ content) [version_ "1.1", width_ "300" , height_ "200"]

contents :: Html ()
contents = do
  rect_ [width_ "100%", height_ "100%", fill_ "red"]
  circle_ [cx_ "150", cy_ "100", r_ "80", fill_ "green"]
  text_ [x_ "150", y_ "125", fontSize_ "60", textAnchor_ "middle", fill_ "white"] "SVG"


main :: IO ()
main = do
  print $ svg contents
