{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Lucid.Svg.Elements
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- SVG elements.
--
-------------------------------------------------------------------------------

module Lucid.Svg.Elements where

import Lucid.Svg.Core

-- | Type of functions that make elements from attributes and child elements.
type ElementBuilder = [Attribute] -> Element -> Element

-- | @DOCTYPE@ element
doctype_ :: Element
doctype_ = makeElementNoEnd "?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\""

-- | @svg@ element + svg 1.1 Attribute
svg11_:: Element -> Element
svg11_ = svg_ [ makeAttribute "xmlns" "http://www.w3.org/2000/svg"
              , makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
              , makeAttribute "version" "1.1" ]

-- | @a@ element
a_ :: ElementBuilder
a_ = element "a"

-- | @altglyph@ element
{-# DEPRECATED altGlyph_ "Removed from web standards." #-}
altGlyph_ :: ElementBuilder
altGlyph_ = element "altGlyph"

-- | @altglyphdef@ element
{-# DEPRECATED altGlyphDef_ "Removed from web standards." #-}
altGlyphDef_ :: ElementBuilder
altGlyphDef_ = element "altGlyphDef"

-- | @altglyphitem@ element
{-# DEPRECATED altGlyphItem_ "Removed from web standards." #-}
altGlyphItem_ :: ElementBuilder
altGlyphItem_ = element "altGlyphItem"

-- | @animate@ element
animate_ :: ElementBuilder
animate_ = element "animate"

-- | @animatecolor@ element
{-# DEPRECATED animateColor_ "Removed from web standards." #-}
animateColor_ :: ElementBuilder
animateColor_ = element "animateColor"

-- | @animatemotion@ element
animateMotion_ :: ElementBuilder
animateMotion_ = element "animateMotion"

-- | @animatetransform@ element
animateTransform_ :: ElementBuilder
animateTransform_ = element "animateTransform"

-- | @circle@ element
circle_ :: ElementBuilder
circle_ = element "circle"

-- | @clipPath@ element or attribute
clipPath_ :: ElementBuilder
clipPath_ = element "clipPath"

-- | @colorProfile@ element
colorProfile_ :: ElementBuilder
colorProfile_ = element "color-profile"

-- | @cursor@ element
cursor_ :: ElementBuilder
cursor_ = element "cursor"

-- | @defs@ element
defs_ :: ElementBuilder
defs_ = element "defs"

-- | @desc@ element
desc_ :: ElementBuilder
desc_ = element "desc"

-- | @ellipse@ element
ellipse_ :: ElementBuilder
ellipse_ = element "ellipse"

-- | @feblend@ element
feBlend_ :: ElementBuilder
feBlend_ = element "feBlend"

-- | @fecolormatrix@ element
feColorMatrix_ :: ElementBuilder
feColorMatrix_ = element "feColorMatrix"

-- | @fecomponenttransfer@ element
feComponentTransfer_ :: ElementBuilder
feComponentTransfer_ = element "feComponentTransfer"

-- | @fecomposite@ element
feComposite_ :: ElementBuilder
feComposite_ = element "feComposite"

-- | @feconvolvematrix@ element
feConvolveMatrix_ :: ElementBuilder
feConvolveMatrix_ = element "feConvolveMatrix"

-- | @fediffuselighting@ element
feDiffuseLighting_ :: ElementBuilder
feDiffuseLighting_ = element "feDiffuseLighting"

-- | @fedisplacementmap@ element
feDisplacementMap_ :: ElementBuilder
feDisplacementMap_ = element "feDisplacementMap"

-- | @fedistantlight@ element
feDistantLight_ :: ElementBuilder
feDistantLight_ = element "feDistantLight"

-- | @feflood@ element
feFlood_ :: ElementBuilder
feFlood_ = element "feFlood"

-- | @fefunca@ element
feFuncA_ :: ElementBuilder
feFuncA_ = element "feFuncA"

-- | @fefuncb@ element
feFuncB_ :: ElementBuilder
feFuncB_ = element "feFuncB"

-- | @fefuncg@ element
feFuncG_ :: ElementBuilder
feFuncG_ = element "feFuncG"

-- | @fefuncr@ element
feFuncR_ :: ElementBuilder
feFuncR_ = element "feFuncR"

-- | @fegaussianblur@ element
feGaussianBlur_ :: ElementBuilder
feGaussianBlur_ = element "feGaussianBlur"

-- | @feimage@ element
feImage_ :: ElementBuilder
feImage_ = element "feImage"

-- | @femerge@ element
feMerge_ :: ElementBuilder
feMerge_ = element "feMerge"

-- | @femergenode@ element
feMergeNode_ :: ElementBuilder
feMergeNode_ = element "feMergeNode"

-- | @femorphology@ element
feMorphology_ :: ElementBuilder
feMorphology_ = element "feMorphology"

-- | @feoffset@ element
feOffset_ :: ElementBuilder
feOffset_ = element "feOffset"

-- | @fepointlight@ element
fePointLight_ :: ElementBuilder
fePointLight_ = element "fePointLight"

-- | @fespecularlighting@ element
feSpecularLighting_ :: ElementBuilder
feSpecularLighting_ = element "feSpecularLighting"

-- | @fespotlight@ element
feSpotLight_ :: ElementBuilder
feSpotLight_ = element "feSpotLight"

-- | @fetile@ element
feTile_ :: ElementBuilder
feTile_ = element "feTile"

-- | @feturbulence@ element
feTurbulence_ :: ElementBuilder
feTurbulence_ = element "feTurbulence"

-- | @filter_@ element
filter_ :: ElementBuilder
filter_ = element "filter"

-- | @font@ element
font_ :: ElementBuilder
font_ = element "font"

-- | @fontFace@ element
fontFace_ :: ElementBuilder
fontFace_ = element "font-face"

-- | @fontFaceFormat@ element
fontFaceFormat_ :: [Attribute] -> Element
fontFaceFormat_ = with $ makeXmlElementNoEnd "font-face-format"

-- | @fontFaceName@ element
fontFaceName_ :: [Attribute] -> Element
fontFaceName_ = with $ makeXmlElementNoEnd "font-face-name"

-- | @fontFaceSrc@ element
fontFaceSrc_ :: ElementBuilder
fontFaceSrc_ = element "font-face-src"

-- | @fontFaceUri@ element
fontFaceUri_ :: ElementBuilder
fontFaceUri_ = element "font-face-uri"

-- | @foreignobject@ element
foreignObject_ :: ElementBuilder
foreignObject_ = element "foreignObject"

-- | @g@ element
g_ :: ElementBuilder
g_ = element "g"

-- | @glyph@ element or attribute
glyph_ :: ElementBuilder
glyph_ = element "glyph"

-- | @glyphref@ element
glyphRef_ :: [Attribute] -> Element
glyphRef_ = with $ makeXmlElementNoEnd "glyphRef"

-- | @hkern@ element
hkern_ :: [Attribute] -> Element
hkern_ = with $ makeXmlElementNoEnd "hkern"

-- | @image@ element
image_ :: ElementBuilder
image_ = element "image"

-- | @line@ element
line_ :: ElementBuilder
line_ = element "line"

-- | @lineargradient@ element
linearGradient_ :: ElementBuilder
linearGradient_ = element "linearGradient"

-- | @marker@ element
marker_ :: ElementBuilder
marker_ = element "marker"

-- | @mask@ element or attribute
mask_ :: ElementBuilder
mask_ = element "mask"

-- | @metadata@ element
metadata_ :: ElementBuilder
metadata_ = element "metadata"

-- | @missingGlyph@ element
missingGlyph_ :: ElementBuilder
missingGlyph_ = element "missing-glyph"

-- | @mpath@ element
mpath_ :: ElementBuilder
mpath_ = element "mpath"

-- | @path@ element
path_ :: ElementBuilder
path_ = element "path"

-- | @pattern@ element
pattern_ :: ElementBuilder
pattern_ = element "pattern"

-- | @polygon@ element
polygon_ :: ElementBuilder
polygon_ = element "polygon"

-- | @polyline@ element
polyline_ :: ElementBuilder
polyline_ = element "polyline"

-- | @radialgradient@ element
radialGradient_ :: ElementBuilder
radialGradient_ = element "radialGradient"

-- | @rect@ element
rect_ :: ElementBuilder
rect_ = element "rect"

-- | @script@ element
script_ :: ElementBuilder
script_ = element "script"

-- | @set@ element
set_ :: ElementBuilder
set_ = element "set"

-- | @stop@ element
stop_ :: ElementBuilder
stop_ = element "stop"

-- | @style@ element
style_ :: ElementBuilder
style_ = element "style"

-- | @svg@ element
svg_ :: ElementBuilder
svg_ = element "svg"

-- | @switch@ element
switch_ :: ElementBuilder
switch_ = element "switch"

-- | @symbol@ element
symbol_ :: ElementBuilder
symbol_ = element "symbol"

-- | @text_@ element
text_ :: ElementBuilder
text_ = element "text"

-- | @textpath@ element
textPath_ :: ElementBuilder
textPath_ = element "textPath"

-- | @title@ element
title_ :: ElementBuilder
title_ = element "title"

-- | @tref@ element
tref_ :: ElementBuilder
tref_ = element "tref"

-- | @tspan@ element
tspan_ :: ElementBuilder
tspan_ = element "tspan"

-- | @use@ element
use_ :: ElementBuilder
use_ = element "use"

-- | @view@ element
view_ :: ElementBuilder
view_ = element "view"

-- | @vkern@ element
vkern_ :: [Attribute] -> Element
vkern_ = with $ makeXmlElementNoEnd "vkern"
