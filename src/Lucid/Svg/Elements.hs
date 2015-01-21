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

import Data.Text (Text)
import Lucid.Base

-- | A type alias for the 'SvgT m a' monad transformer.
type SvgT = HtmlT

-- | Make an HTML builder for elements with no content, only attributes.
--   SVG circle for example.
makeElementNoContent :: Monad m => Text -> SvgT m ()
makeElementNoContent name = makeElement name ""

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
altGlyph_ = with $ makeElementNoContent "altGlyph"

-- | @altglyphdef@ element
altGlyphDef_ :: Monad m => [Attribute] -> SvgT m ()
altGlyphDef_ = with $ makeElementNoContent "altGlyphDef" 

-- | @altglyphitem@ element
altGlyphItem_ :: Monad m => [Attribute] -> SvgT m ()
altGlyphItem_ = with $ makeElementNoContent "altGlyphItem" 

-- | @animate@ element
animate_ :: Monad m => [Attribute] -> SvgT m ()
animate_ = with $ makeElementNoContent "animate" 

-- | @animatecolor@ element
animateColor_ :: Monad m => [Attribute] -> SvgT m ()
animateColor_ = with $ makeElementNoContent "animateColor" 

-- | @animatemotion@ element
animateMotion_ :: Monad m => [Attribute] -> SvgT m ()
animateMotion_ = with $ makeElementNoContent "animateMotion" 

-- | @animatetransform@ element
animateTransform_ :: Monad m => [Attribute] -> SvgT m ()
animateTransform_ = with $ makeElementNoContent "animateTransform" 

-- | @circle@ element
circle_ :: Monad m => [Attribute] -> SvgT m ()
circle_ = with $ makeElementNoContent "circle" 

-- | @clipPath@ element or attribute
clipPath_ :: Term arg result => arg -> result
clipPath_ = term "clippath"

-- | @colorProfile@ element 
colorProfile_ :: Monad m => [Attribute] -> SvgT m ()
colorProfile_ = with $ makeElementNoContent "color-profile" 

-- | @cursor@ element
cursor_ :: Monad m => [Attribute] -> SvgT m ()
cursor_ = with $ makeElementNoContent "cursor" 

-- | @defs@ element
defs_ :: Term arg result => arg -> result
defs_ = term "defs"

-- | @desc@ element
desc_ :: Monad m => [Attribute] -> SvgT m ()
desc_ = with $ makeElementNoContent "desc" 

-- | @ellipse@ element
ellipse_ :: Monad m => [Attribute] -> SvgT m ()
ellipse_ = with $ makeElementNoContent "ellipse" 

-- | @feblend@ element
feBlend_ :: Monad m => [Attribute] -> SvgT m ()
feBlend_ = with $ makeElementNoContent "feBlend" 

-- | @fecolormatrix@ element
feColorMatrix_ :: Monad m => [Attribute] -> SvgT m ()
feColorMatrix_ = with $ makeElementNoContent "feColorMatrix" 

-- | @fecomponenttransfer@ element
feComponentTransfer_ :: Monad m => [Attribute] -> SvgT m ()
feComponentTransfer_ = with $ makeElementNoContent "feComponentTransfer" 

-- | @fecomposite@ element
feComposite_ :: Monad m => [Attribute] -> SvgT m ()
feComposite_ = with $ makeElementNoContent "feComposite" 

-- | @feconvolvematrix@ element
feConvolveMatrix_ :: Monad m => [Attribute] -> SvgT m ()
feConvolveMatrix_ = with $ makeElementNoContent "feConvolveMatrix" 

-- | @fediffuselighting@ element
feDiffuseLighting_ :: Monad m => [Attribute] -> SvgT m ()
feDiffuseLighting_ = with $ makeElementNoContent "feDiffuseLighting" 

-- | @fedisplacementmap@ element
feDisplacementMap_ :: Monad m => [Attribute] -> SvgT m ()
feDisplacementMap_ = with $ makeElementNoContent "feDisplacementMap" 

-- | @fedistantlight@ element
feDistantLight_ :: Monad m => [Attribute] -> SvgT m ()
feDistantLight_ = with $ makeElementNoContent "feDistantLight" 

-- | @feflood@ element
feFlood_ :: Monad m => [Attribute] -> SvgT m ()
feFlood_ = with $ makeElementNoContent "feFlood" 

-- | @fefunca@ element
feFuncA_ :: Monad m => [Attribute] -> SvgT m ()
feFuncA_ = with $ makeElementNoContent "feFuncA" 

-- | @fefuncb@ element
feFuncB_ :: Monad m => [Attribute] -> SvgT m ()
feFuncB_ = with $ makeElementNoContent "feFuncB" 

-- | @fefuncg@ element
feFuncG_ :: Monad m => [Attribute] -> SvgT m ()
feFuncG_ = with $ makeElementNoContent "feFuncG" 

-- | @fefuncr@ element
feFuncR_ :: Monad m => [Attribute] -> SvgT m ()
feFuncR_ = with $ makeElementNoContent "feFuncR" 

-- | @fegaussianblur@ element
feGaussianBlur_ :: Monad m => [Attribute] -> SvgT m ()
feGaussianBlur_ = with $ makeElementNoContent "feGaussianBlur" 

-- | @feimage@ element
feImage_ :: Monad m => [Attribute] -> SvgT m ()
feImage_ = with $ makeElementNoContent "feImage" 

-- | @femerge@ element
feMerge_ :: Monad m => [Attribute] -> SvgT m ()
feMerge_ = with $ makeElementNoContent "feMerge" 

-- | @femergenode@ element
feMergeNode_ :: Monad m => [Attribute] -> SvgT m ()
feMergeNode_ = with $ makeElementNoContent "feMergeNode" 

-- | @femorphology@ element
feMorphology_ :: Monad m => [Attribute] -> SvgT m ()
feMorphology_ = with $ makeElementNoContent "feMorphology" 

-- | @feoffset@ element
feOffset_ :: Monad m => [Attribute] -> SvgT m ()
feOffset_ = with $ makeElementNoContent "feOffset" 

-- | @fepointlight@ element
fePointLight_ :: Monad m => [Attribute] -> SvgT m ()
fePointLight_ = with $ makeElementNoContent "fePointLight" 

-- | @fespecularlighting@ element
feSpecularLighting_ :: Monad m => [Attribute] -> SvgT m ()
feSpecularLighting_ = with $ makeElementNoContent "feSpecularLighting" 

-- | @fespotlight@ element
feSpotLight_ :: Monad m => [Attribute] -> SvgT m ()
feSpotLight_ = with $ makeElementNoContent "feSpotLight" 

-- | @fetile@ element
feTile_ :: Monad m => [Attribute] -> SvgT m ()
feTile_ = with $ makeElementNoContent "feTile" 

-- | @feturbulence@ element
feTurbulence_ :: Monad m => [Attribute] -> SvgT m ()
feTurbulence_ = with $ makeElementNoContent "feTurbulence" 

-- | @filter_@ element
filter_ :: Monad m => [Attribute] -> SvgT m ()
filter_ = with $ makeElementNoContent "filter" 

-- | @font@ element
font_ :: Monad m => [Attribute] -> SvgT m ()
font_ = with $ makeElementNoContent "font" 

-- | @fontFace@ element
fontFace_ :: Monad m => [Attribute] -> SvgT m ()
fontFace_ = with $ makeElementNoContent "font-face" 

-- | @fontFaceFormat@ element
fontFaceFormat_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceFormat_ = with $ makeElementNoContent "font-face-format" 

-- | @fontFaceName@ element
fontFaceName_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceName_ = with $ makeElementNoContent "font-face-name" 

-- | @fontFaceSrc@ element
fontFaceSrc_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceSrc_ = with $ makeElementNoContent "font-face-src" 

-- | @fontFaceUri@ element
fontFaceUri_ :: Monad m => [Attribute] -> SvgT m ()
fontFaceUri_ = with $ makeElementNoContent "font-face-uri" 

-- | @foreignobject@ element
foreignObject_ :: Monad m => [Attribute] -> SvgT m ()
foreignObject_ = with $ makeElementNoContent "foreignObject" 

-- | @g@ element
g_ :: Term arg result => arg -> result
g_ = term "g"

-- | @glyph@ element or attribute
glyph_ :: Term arg result => arg -> result
glyph_ = term "glyph"

-- | @glyphref@ element
glyphRef_ :: Monad m => [Attribute] -> SvgT m ()
glyphRef_ = with $ makeElementNoContent "glyphRef" 

-- | @hkern@ element
hkern_ :: Monad m => [Attribute] -> SvgT m ()
hkern_ = with $ makeElementNoContent "hkern" 

-- | @image@ element
image_ :: Monad m => [Attribute] -> SvgT m ()
image_ = with $ makeElementNoContent "image" 

-- | @line@ element
line_ :: Monad m => [Attribute] -> SvgT m ()
line_ = with $ makeElementNoContent "line" 

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
metadata_ = with $ makeElementNoContent "metadata" 

-- | @missingGlyph@ element
missingGlyph_ :: Term arg result => arg -> result
missingGlyph_ = term "missing-glyph"

-- | @mpath@ element
mpath_ :: Monad m => [Attribute] -> SvgT m ()
mpath_ = with $ makeElementNoContent "mpath" 

-- | @path@ element
path_ :: Monad m => [Attribute] -> SvgT m ()
path_ = with $ makeElementNoContent "path" 

-- | @pattern@ element
pattern_ :: Term arg result => arg -> result
pattern_ = term "pattern"

-- | @polygon@ element
polygon_ :: Monad m => [Attribute] -> SvgT m ()
polygon_ = with $ makeElementNoContent "polygon" 

-- | @polyline@ element
polyline_ :: Monad m => [Attribute] -> SvgT m ()
polyline_ = with $ makeElementNoContent "polyline" 

-- | @radialgradient@ element
radialGradient_ :: Term arg result => arg -> result
radialGradient_ = term "radialGradient"

-- | @rect@ element
rect_ :: Monad m => [Attribute] -> SvgT m ()
rect_ = with $ makeElementNoContent "rect"

-- | @script@ element
script_ :: Monad m => [Attribute] -> SvgT m ()
script_ = with $ makeElementNoContent "script" 

-- | @set@ element
set_ :: Monad m => [Attribute] -> SvgT m ()
set_ = with $ makeElementNoContent "set" 

-- | @stop@ element
stop_ :: Monad m => [Attribute] -> SvgT m ()
stop_ = with $ makeElementNoContent "stop" 

-- | @style@ element
style_ :: Monad m => [Attribute] -> SvgT m ()
style_ = with $ makeElementNoContent "style" 

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
textPath_ = with $ makeElementNoContent "textPath" 

-- | @title@ element
title_ :: Monad m => [Attribute] -> SvgT m ()
title_ = with $ makeElementNoContent "title" 

-- | @tref@ element
tref_ :: Monad m => [Attribute] -> SvgT m ()
tref_ = with $ makeElementNoContent "tref" 

-- | @tspan@ element
tspan_ :: Monad m => [Attribute] -> SvgT m ()
tspan_ = with $ makeElementNoContent "tspan" 

-- | @use@ element
use_ :: Monad m => [Attribute] -> SvgT m ()
use_ = with $ makeElementNoContent "use" 

-- | @view@ element
view_ :: Monad m => [Attribute] -> SvgT m ()
view_ = with $ makeElementNoContent "view" 

-- | @vkern@ element
vkern_ :: Monad m => [Attribute] -> SvgT m ()
vkern_ = with $ makeElementNoContent "vkern" 
