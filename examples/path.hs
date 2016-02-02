{-# LANGUAGE OverloadedStrings #-}

import Lucid.Svg
import Data.Monoid
import Data.Text.Lazy as T

svg :: Element -> Element
svg content = do
  doctype_
  with (svg11_ content) [Width <-- "325" , Height <-- "325"]

contents :: Element
contents = do
  path_ (
    [ D <-- (mA 10 80 <> qA 52.5 10 95 80 <> tA 180 80 <> z)
    , Stroke <-- "blue"
    , Fill <-- "orange"
    ]) nil

main :: IO ()
main = do
  putStrLn . T.unpack . renderText $ svg contents
