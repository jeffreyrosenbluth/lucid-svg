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

import Lucid.Base

-- | A type alias for the 'SvgT m a' monad transformer.
type SvgT = HtmlT

-- | @DOCTYPE@ element
doctype_ :: Monad m => SvgT m ()
doctype_ = makeElementNoEnd "?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"" 

-- | @svg@ element + svg 1.1 attributes
svg11_:: Term [Attribute] (s -> t) => s -> t
svg11_ m = svg_ [ makeAttribute "xmlns" "http://www.w3.org/2000/svg"
                , makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
                , makeAttribute "version" "1.1" ]
           m

-- | @a@ element
a_ :: Term arg result => arg -> result
a_ = term "a"

-- | @altglyph@ element
altGlyph_ :: Monad m => [Attribute] -> SvgT m ()
altGlyph_ = with $ makeElementSvg "altGlyph"

-- | @altglyphdef@ element
altGlyphDef_ :: Monad m => [Attribute] -> SvgT m ()
altGlyphDef_ = with $ makeElementSvg "altGlyphDef" 

-- | @altglyphitem@ element
altGlyphItem_ :: Monad m => [Attribute] -> SvgT m ()
altGlyphItem_ = with $ makeElementSvg "altGlyphItem" 

-- | @animate@ element
animate_ :: Monad m => [Attribute] -> SvgT m ()
animate_ = with $ makeElementSvg "animate" 

-- | @animatecolor@ element
animateColor_ :: Monad m => [Attribute] -> SvgT m ()
animateColor_ = with $ makeElementSvg "animateColor" 

-- | @animatemotion@ element
animateMotion_ :: Monad m => [Attribute] -> SvgT m ()
animateMotion_ = with $ makeElementSvg "animateMotion" 

-- | @animatetransform@ element
animateTransform_ :: Monad m => [Attribute] -> SvgT m ()
animateTransform_ = with $ makeElementSvg "animateTransform" 

-- | @circle@ element
circle_ :: Monad m => [Attribute] -> SvgT m ()
circle_ = with $ makeElementSvg "circle" 

-- | @clipPath@ element or attribute
clipPath_ :: Term arg result => arg -> result
clipPath_ = term "clippath"

-- | @colorProfile@ element 
colorProfile_ :: Monad m => [Attribute] -> SvgT m ()
colorProfile_ = with $ makeElementSvg "color-profile" 

-- | @cursor@ element
cursor_ :: Monad m => [Attribute] -> SvgT m ()
cursor_ = with $ makeElementSvg "cursor" 

-- | @defs@ element
defs_ :: Term arg result => arg -> result
defs_ = term "defs"

-- | @desc@ element
desc_ :: Monad m => [Attribute] -> SvgT m ()
desc_ = with $ makeElementSvg "desc" 

-- | @ellipse@ element
ellipse_ :: Monad m => [Attribute] -> SvgT m ()
ellipse_ = with $ makeElementSvg "ellipse" 

-- | @feblend@ element
feBlend_ :: Monad m => [Attribute] -> SvgT m ()
feBlend_ = with $ makeElementSvg "feBlend" 

-- | @fecolormatrix@ element
feColorMatrix_ :: Monad m => [Attribute] -> SvgT m ()
feColorMatrix_ = with $ makeElementSvg "feColorMatrix" 

-- | @fecomponenttransfer@ element
feComponentTransfer_ :: Monad m => [Attribute] -> SvgT m ()
feComponentTransfer_ = with $ makeElementSvg "feComponentTransfer" 

-- | @fecomposite@ element
feComposite_ :: Monad m => [Attribute] -> SvgT m ()
feComposite_ = with $ makeElementSvg "feComposite" 

-- | @feconvolvematrix@ element
feConvolveMatrix_ :: Monad m => [Attribute] -> SvgT m ()
feConvolveMatrix_ = with $ makeElementSvg "feConvolveMatrix" 

-- | @fediffuselighting@ element
feDiffuseLighting_ :: Monad m => [Attribute] -> SvgT m ()
feDiffuseLighting_ = with $ makeElementSvg "feDiffuseLighting" 

-- | @fedisplacementmap@ element
feDisplacementMap_ :: Monad m => [Attribute] -> SvgT m ()
feDisplacementMap_ = with $ makeElementSvg "feDisplacementMap" 

-- | @fedistantlight@ element
feDistantLight_ :: Monad m => [Attribute] -> SvgT m ()
feDistantLight_ = with $ makeElementSvg "feDistantLight" 

-- | @feflood@ element
feFlood_ :: Monad m => [Attribute] -> SvgT m ()
feFlood_ = with $ makeElementSvg "feFlood" 

-- | @fefunca@ element
feFuncA_ :: Monad m => [Attribute] -> SvgT m ()
feFuncA_ = with $ makeElementSvg "feFuncA" 

-- | @fefuncb@ element
feFuncB_ :: Monad m => [Attribute] -> SvgT m ()
feFuncB_ = with $ makeElementSvg "feFuncB" 

-- | @fefuncg@ element
feFuncG_ :: Monad m => [Attribute] -> SvgT m ()
feFuncG_ = with $ makeElementSvg "feFuncG" 

-- | @fefuncr@ element
feFuncR_ :: Monad m => [Attribute] -> SvgT m ()
feFuncR_ = with $ makeElementSvg "feFuncR" 

-- | @fegaussianblur@ element
feGaussianBlur_ :: Monad m => [Attribute] -> SvgT m ()
feGaussianBlur_ = with $ makeElementSvg "feGaussianBlur" 

-- | @feimage@ element
feImage_ :: Monad m => [Attribute] -> SvgT m ()
feImage_ = with $ makeElementSvg "feImage" 

-- | @femerge@ element
feMerge_ :: Monad m => [Attribute] -> SvgT m ()
feMerge_ = with $ makeElementSvg "feMerge" 

-- | @femergenode@ element
feMergeNode_ :: Monad m => [Attribute] -> SvgT m ()
feMergeNode_ = with $ makeElementSvg "feMergeNode" 

-- | @femorphology@ element
feMorphology_ :: Monad m => [Attribute] -> SvgT m ()
feMorphology_ = with $ makeElementSvg "feMorphology" 

-- | @feoffset@ element
feOffset_ :: Monad m => [Attribute] -> SvgT m ()
feOffset_ = with $ makeElementSvg "feOffset" 

-- | @fepointlight@ element
fePointLight_ :: Monad m => [Attribute] -> SvgT m ()
fePointLight_ = with $ makeElementSvg "fePointLight" 

-- | @fespecularlighting@ element
feSpecularLighting_ :: Monad m => [Attribute] -> SvgT m ()
feSpecularLighting_ = with $ makeElementSvg "feSpecularLighting" 

-- | @fespotlight@ element
feSpotLight_ :: Monad m => [Attribute] -> SvgT m ()
feSpotLight_ = with $ makeElementSvg "feSpotLight" 

-- | @fetile@ element
feTile_ :: Monad m => [Attribute] -> SvgT m ()
feTile_ = with $ makeElementSvg "feTile" 

-- | @feturbulence@ element
feTurbulence_ :: Monad m => [Attribute] -> SvgT m ()
feTurbulence_ = with $ makeElementSvg "feTurbulence" 

-- | @filter_@ element
filter_ :: Monad m => [Attribute] -> SvgT m ()
filter_ = with $ makeElementSvg "filter" 

-- | @font@ element
font_ :: Monad m => [Attribute] -> SvgT m ()
font_ = with $ makeElementSvg "font" 

-- | @fontFace@ element
fontFace_ :: Monad m => [Attribute] -> SvgT m ()
fontFace_ = with $ makeElementSvg "font-face" 

-- | @fontFaceFormat@ element
fontFaceFormat_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceFormat_ = with $ makeElementSvg "font-face-format" 

-- | @fontFaceName@ element
fontFaceName_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceName_ = with $ makeElementSvg "font-face-name" 

-- | @fontFaceSrc@ element
fontFaceSrc_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceSrc_ = with $ makeElementSvg "font-face-src" 

-- | @fontFaceUri@ element
fontFaceUri_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceUri_ = with $ makeElementSvg "font-face-uri" 

-- | @foreignobject@ element
foreignObject_ :: Monad m => [Attribute] -> SvgT m ()
foreignObject_ = with $ makeElementSvg "foreignObject" 

-- | @g@ element
g_ :: Term arg result => arg -> result
g_ = term "g"

-- | @glyph@ element or attribute
glyph_ :: Term arg result => arg -> result
glyph_ = term "glyph"

-- | @glyphref@ element
glyphRef_ :: Monad m => [Attribute] -> SvgT m ()
glyphRef_ = with $ makeElementSvg "glyphRef" 

-- | @hkern@ element
hkern_ :: Monad m => [Attribute] -> SvgT m ()
hkern_ = with $ makeElementSvg "hkern" 

-- | @image@ element
image_ :: Monad m => [Attribute] -> SvgT m ()
image_ = with $ makeElementSvg "image" 

-- | @line@ element
line_ :: Monad m => [Attribute] -> SvgT m ()
line_ = with $ makeElementSvg "line" 

-- | @lineargradient@ element
linearGradient_ :: Term arg result => arg -> result
linearGradient_ = term "linearGradient"

-- | @marker@ element
marker_ :: Term arg result => arg -> result
marker_ = term "marker"

-- | @mask@ element or attribute
mask_ :: Term arg result => arg -> result
mask_ = term "mask"

-- | @metadata@ element
metadata_ :: Monad m => [Attribute] -> SvgT m ()
metadata_ = with $ makeElementSvg "metadata" 

-- | @missingGlyph@ element
missingGlyph_ :: Term arg result => arg -> result
missingGlyph_ = term "missing-glyph"

-- | @mpath@ element
mpath_ :: Monad m => [Attribute] -> SvgT m ()
mpath_ = with $ makeElementSvg "mpath" 

-- | @path@ element
path_ :: Monad m => [Attribute] -> SvgT m ()
path_ = with $ makeElementSvg "path" 

-- | @pattern@ element
pattern_ :: Term arg result => arg -> result
pattern_ = term "pattern"

-- | @polygon@ element
polygon_ :: Monad m => [Attribute] -> SvgT m ()
polygon_ = with $ makeElementSvg "polygon" 

-- | @polyline@ element
polyline_ :: Monad m => [Attribute] -> SvgT m ()
polyline_ = with $ makeElementSvg "polyline" 

-- | @radialgradient@ element
radialGradient_ :: Term arg result => arg -> result
radialGradient_ = term "radialGradient"

-- | @rect@ element
rect_ :: Monad m => [Attribute] -> SvgT m ()
rect_ = with $ makeElementSvg "rect"

-- | @script@ element
script_ :: Monad m => [Attribute] -> SvgT m ()
script_ = with $ makeElementSvg "script" 

-- | @set@ element
set_ :: Monad m => [Attribute] -> SvgT m ()
set_ = with $ makeElementSvg "set" 

-- | @stop@ element
stop_ :: Monad m => [Attribute] -> SvgT m ()
stop_ = with $ makeElementSvg "stop" 

-- | @style@ element
style_ :: Monad m => [Attribute] -> SvgT m ()
style_ = with $ makeElementSvg "style" 

-- | @svg@ element
svg_ :: Term arg result => arg -> result
svg_ = term "svg"

-- | @switch@ element
switch_ :: Term arg result => arg -> result
switch_ = term "switch"

-- | @symbol@ element
symbol_ :: Term arg result => arg -> result
symbol_ = term "symbol"

-- | @text_@ element
text_ :: Term arg result => arg -> result
text_ = term "text"

-- | @textpath@ element
textPath_ :: Monad m => [Attribute] -> SvgT m ()
textPath_ = with $ makeElementSvg "textPath" 

-- | @title@ element
title_ :: Monad m => [Attribute] -> SvgT m ()
title_ = with $ makeElementSvg "title" 

-- | @tref@ element
tref_ :: Monad m => [Attribute] -> SvgT m ()
tref_ = with $ makeElementSvg "tref" 

-- | @tspan@ element
tspan_ :: Monad m => [Attribute] -> SvgT m ()
tspan_ = with $ makeElementSvg "tspan" 

-- | @use@ element
use_ :: Monad m => [Attribute] -> SvgT m ()
use_ = with $ makeElementSvg "use" 

-- | @view@ element
view_ :: Monad m => [Attribute] -> SvgT m ()
view_ = with $ makeElementSvg "view" 

-- | @vkern@ element
vkern_ :: Monad m => [Attribute] -> SvgT m ()
vkern_ = with $ makeElementSvg "vkern" 
