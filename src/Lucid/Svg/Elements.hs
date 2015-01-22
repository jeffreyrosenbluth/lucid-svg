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
altGlyph_ = with $ makeElementNoEnd "altGlyph"

-- | @altglyphdef@ element
altGlyphDef_ :: Monad m => [Attribute] -> SvgT m ()
altGlyphDef_ = with $ makeElementNoEnd "altGlyphDef" 

-- | @altglyphitem@ element
altGlyphItem_ :: Monad m => [Attribute] -> SvgT m ()
altGlyphItem_ = with $ makeElementNoEnd "altGlyphItem" 

-- | @animate@ element
animate_ :: Monad m => [Attribute] -> SvgT m ()
animate_ = with $ makeElementNoEnd "animate" 

-- | @animatecolor@ element
animateColor_ :: Monad m => [Attribute] -> SvgT m ()
animateColor_ = with $ makeElementNoEnd "animateColor" 

-- | @animatemotion@ element
animateMotion_ :: Monad m => [Attribute] -> SvgT m ()
animateMotion_ = with $ makeElementNoEnd "animateMotion" 

-- | @animatetransform@ element
animateTransform_ :: Monad m => [Attribute] -> SvgT m ()
animateTransform_ = with $ makeElementNoEnd "animateTransform" 

-- | @circle@ element
circle_ :: Monad m => [Attribute] -> SvgT m ()
circle_ = with $ makeElementNoEnd "circle" 

-- | @clipPath@ element or attribute
clipPath_ :: Term arg result => arg -> result
clipPath_ = term "clippath"

-- | @colorProfile@ element 
colorProfile_ :: Monad m => [Attribute] -> SvgT m ()
colorProfile_ = with $ makeElementNoEnd "color-profile" 

-- | @cursor@ element
cursor_ :: Monad m => [Attribute] -> SvgT m ()
cursor_ = with $ makeElementNoEnd "cursor" 

-- | @defs@ element
defs_ :: Term arg result => arg -> result
defs_ = term "defs"

-- | @desc@ element
desc_ :: Monad m => [Attribute] -> SvgT m ()
desc_ = with $ makeElementNoEnd "desc" 

-- | @ellipse@ element
ellipse_ :: Monad m => [Attribute] -> SvgT m ()
ellipse_ = with $ makeElementNoEnd "ellipse" 

-- | @feblend@ element
feBlend_ :: Monad m => [Attribute] -> SvgT m ()
feBlend_ = with $ makeElementNoEnd "feBlend" 

-- | @fecolormatrix@ element
feColorMatrix_ :: Monad m => [Attribute] -> SvgT m ()
feColorMatrix_ = with $ makeElementNoEnd "feColorMatrix" 

-- | @fecomponenttransfer@ element
feComponentTransfer_ :: Monad m => [Attribute] -> SvgT m ()
feComponentTransfer_ = with $ makeElementNoEnd "feComponentTransfer" 

-- | @fecomposite@ element
feComposite_ :: Monad m => [Attribute] -> SvgT m ()
feComposite_ = with $ makeElementNoEnd "feComposite" 

-- | @feconvolvematrix@ element
feConvolveMatrix_ :: Monad m => [Attribute] -> SvgT m ()
feConvolveMatrix_ = with $ makeElementNoEnd "feConvolveMatrix" 

-- | @fediffuselighting@ element
feDiffuseLighting_ :: Monad m => [Attribute] -> SvgT m ()
feDiffuseLighting_ = with $ makeElementNoEnd "feDiffuseLighting" 

-- | @fedisplacementmap@ element
feDisplacementMap_ :: Monad m => [Attribute] -> SvgT m ()
feDisplacementMap_ = with $ makeElementNoEnd "feDisplacementMap" 

-- | @fedistantlight@ element
feDistantLight_ :: Monad m => [Attribute] -> SvgT m ()
feDistantLight_ = with $ makeElementNoEnd "feDistantLight" 

-- | @feflood@ element
feFlood_ :: Monad m => [Attribute] -> SvgT m ()
feFlood_ = with $ makeElementNoEnd "feFlood" 

-- | @fefunca@ element
feFuncA_ :: Monad m => [Attribute] -> SvgT m ()
feFuncA_ = with $ makeElementNoEnd "feFuncA" 

-- | @fefuncb@ element
feFuncB_ :: Monad m => [Attribute] -> SvgT m ()
feFuncB_ = with $ makeElementNoEnd "feFuncB" 

-- | @fefuncg@ element
feFuncG_ :: Monad m => [Attribute] -> SvgT m ()
feFuncG_ = with $ makeElementNoEnd "feFuncG" 

-- | @fefuncr@ element
feFuncR_ :: Monad m => [Attribute] -> SvgT m ()
feFuncR_ = with $ makeElementNoEnd "feFuncR" 

-- | @fegaussianblur@ element
feGaussianBlur_ :: Monad m => [Attribute] -> SvgT m ()
feGaussianBlur_ = with $ makeElementNoEnd "feGaussianBlur" 

-- | @feimage@ element
feImage_ :: Monad m => [Attribute] -> SvgT m ()
feImage_ = with $ makeElementNoEnd "feImage" 

-- | @femerge@ element
feMerge_ :: Monad m => [Attribute] -> SvgT m ()
feMerge_ = with $ makeElementNoEnd "feMerge" 

-- | @femergenode@ element
feMergeNode_ :: Monad m => [Attribute] -> SvgT m ()
feMergeNode_ = with $ makeElementNoEnd "feMergeNode" 

-- | @femorphology@ element
feMorphology_ :: Monad m => [Attribute] -> SvgT m ()
feMorphology_ = with $ makeElementNoEnd "feMorphology" 

-- | @feoffset@ element
feOffset_ :: Monad m => [Attribute] -> SvgT m ()
feOffset_ = with $ makeElementNoEnd "feOffset" 

-- | @fepointlight@ element
fePointLight_ :: Monad m => [Attribute] -> SvgT m ()
fePointLight_ = with $ makeElementNoEnd "fePointLight" 

-- | @fespecularlighting@ element
feSpecularLighting_ :: Monad m => [Attribute] -> SvgT m ()
feSpecularLighting_ = with $ makeElementNoEnd "feSpecularLighting" 

-- | @fespotlight@ element
feSpotLight_ :: Monad m => [Attribute] -> SvgT m ()
feSpotLight_ = with $ makeElementNoEnd "feSpotLight" 

-- | @fetile@ element
feTile_ :: Monad m => [Attribute] -> SvgT m ()
feTile_ = with $ makeElementNoEnd "feTile" 

-- | @feturbulence@ element
feTurbulence_ :: Monad m => [Attribute] -> SvgT m ()
feTurbulence_ = with $ makeElementNoEnd "feTurbulence" 

-- | @filter_@ element
filter_ :: Monad m => [Attribute] -> SvgT m ()
filter_ = with $ makeElementNoEnd "filter" 

-- | @font@ element
font_ :: Monad m => [Attribute] -> SvgT m ()
font_ = with $ makeElementNoEnd "font" 

-- | @fontFace@ element
fontFace_ :: Monad m => [Attribute] -> SvgT m ()
fontFace_ = with $ makeElementNoEnd "font-face" 

-- | @fontFaceFormat@ element
fontFaceFormat_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceFormat_ = with $ makeElementNoEnd "font-face-format" 

-- | @fontFaceName@ element
fontFaceName_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceName_ = with $ makeElementNoEnd "font-face-name" 

-- | @fontFaceSrc@ element
fontFaceSrc_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceSrc_ = with $ makeElementNoEnd "font-face-src" 

-- | @fontFaceUri@ element
fontFaceUri_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceUri_ = with $ makeElementNoEnd "font-face-uri" 

-- | @foreignobject@ element
foreignObject_ :: Monad m => [Attribute] -> SvgT m ()
foreignObject_ = with $ makeElementNoEnd "foreignObject" 

-- | @g@ element
g_ :: Term arg result => arg -> result
g_ = term "g"

-- | @glyph@ element or attribute
glyph_ :: Term arg result => arg -> result
glyph_ = term "glyph"

-- | @glyphref@ element
glyphRef_ :: Monad m => [Attribute] -> SvgT m ()
glyphRef_ = with $ makeElementNoEnd "glyphRef" 

-- | @hkern@ element
hkern_ :: Monad m => [Attribute] -> SvgT m ()
hkern_ = with $ makeElementNoEnd "hkern" 

-- | @image@ element
image_ :: Monad m => [Attribute] -> SvgT m ()
image_ = with $ makeElementNoEnd "image" 

-- | @line@ element
line_ :: Monad m => [Attribute] -> SvgT m ()
line_ = with $ makeElementNoEnd "line" 

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
metadata_ = with $ makeElementNoEnd "metadata" 

-- | @missingGlyph@ element
missingGlyph_ :: Term arg result => arg -> result
missingGlyph_ = term "missing-glyph"

-- | @mpath@ element
mpath_ :: Monad m => [Attribute] -> SvgT m ()
mpath_ = with $ makeElementNoEnd "mpath" 

-- | @path@ element
path_ :: Monad m => [Attribute] -> SvgT m ()
path_ = with $ makeElementNoEnd "path" 

-- | @pattern@ element
pattern_ :: Term arg result => arg -> result
pattern_ = term "pattern"

-- | @polygon@ element
polygon_ :: Monad m => [Attribute] -> SvgT m ()
polygon_ = with $ makeElementNoEnd "polygon" 

-- | @polyline@ element
polyline_ :: Monad m => [Attribute] -> SvgT m ()
polyline_ = with $ makeElementNoEnd "polyline" 

-- | @radialgradient@ element
radialGradient_ :: Term arg result => arg -> result
radialGradient_ = term "radialGradient"

-- | @rect@ element
rect_ :: Monad m => [Attribute] -> SvgT m ()
rect_ = with $ makeElementNoEnd "rect"

-- | @script@ element
script_ :: Monad m => [Attribute] -> SvgT m ()
script_ = with $ makeElementNoEnd "script" 

-- | @set@ element
set_ :: Monad m => [Attribute] -> SvgT m ()
set_ = with $ makeElementNoEnd "set" 

-- | @stop@ element
stop_ :: Monad m => [Attribute] -> SvgT m ()
stop_ = with $ makeElementNoEnd "stop" 

-- | @style@ element
style_ :: Monad m => [Attribute] -> SvgT m ()
style_ = with $ makeElementNoEnd "style" 

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
textPath_ = with $ makeElementNoEnd "textPath" 

-- | @title@ element
title_ :: Monad m => [Attribute] -> SvgT m ()
title_ = with $ makeElementNoEnd "title" 

-- | @tref@ element
tref_ :: Monad m => [Attribute] -> SvgT m ()
tref_ = with $ makeElementNoEnd "tref" 

-- | @tspan@ element
tspan_ :: Monad m => [Attribute] -> SvgT m ()
tspan_ = with $ makeElementNoEnd "tspan" 

-- | @use@ element
use_ :: Monad m => [Attribute] -> SvgT m ()
use_ = with $ makeElementNoEnd "use" 

-- | @view@ element
view_ :: Monad m => [Attribute] -> SvgT m ()
view_ = with $ makeElementNoEnd "view" 

-- | @vkern@ element
vkern_ :: Monad m => [Attribute] -> SvgT m ()
vkern_ = with $ makeElementNoEnd "vkern" 
