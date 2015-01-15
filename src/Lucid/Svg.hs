{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-type-defaults    #-}

module Lucid.Svg
  (
    Svg
  , doctype_
  , svg11_
  )
  where

import Lucid.Base
import Lucid.Svg.Elements

import Data.Functor.Identity

type Svg = HtmlT Identity

-- | @DOCTYPE@ element
doctype_ :: Monad m => HtmlT m ()
doctype_ = makeElementNoEnd "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" 

-- | @svg@ element + svg 1.1 attributes
svg11_:: Term [Attribute] (s -> t) => s -> t
svg11_ m = svg_ [ makeAttribute "xmlns" "http://www.w3.org/2000/svg"
                , makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"]
           m


