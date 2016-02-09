lucid-svg [![Hackage](https://img.shields.io/hackage/v/lucid-svg.svg?style=flat)](https://hackage.haskell.org/package/lucid-svg)
=========
Simple DSL for writing SVG, in the spirit of lucid.

## Example

``` haskell
{-# LANGUAGE OverloadedStrings #-}

import Lucid.Svg

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version <<- "1.1", Width <<- "300", Height <<- "200"]

contents :: Element
contents =
     rect_   [ Width <<- "100%", Height <<- "100%", "red" ->> Fill]
  <> circle_ [ Cx <<- "150", Cy <<- "100", R <<- "80", Fill <<- "green"]
  <> text_   [ X <<- "150", Y <<- "125", Font_size <<- "60"
             , Text_anchor <<- "middle", Fill <<- "white"] "SVG"

main :: IO ()
main = do
  print $ svg contents
```

![SVG](http://i.imgur.com/dXu84xR.png)
