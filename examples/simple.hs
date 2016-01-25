{-# LANGUAGE OverloadedStrings #-}

import Lucid.Svg
import Data.Monoid
import Data.Text.Lazy as T

svg :: Element -> Element
svg content =
     doctype_
  <> with (svg11_ content) [version_ "1.1", width_ "300" , height_ "200"]

contents :: Element
contents =
     rect_   [width_ "100%", height_ "100%", fill_ "red"]
  <> circle_ [cx_ "150", cy_ "100", r_ "80", fill_ "green"]
  <> text_   [x_ "150", y_ "125", font_size_ "60", text_anchor_ "middle", fill_ "white"] "SVG"


main :: IO ()
main = do
  putStrLn . T.unpack . render $ svg contents
