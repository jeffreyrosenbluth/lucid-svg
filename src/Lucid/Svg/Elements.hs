{-# LANGUAGE OverloadedStrings #-}

module Lucid.Svg.Elements where 

import Data.Text (Text)
import Lucid.Base

-- | Make an HTML builder for elements with no content, only attributes.
--   SVG circle for example.
makeElementNoContent :: Monad m => Text -> HtmlT m ()
makeElementNoContent name = makeElement name ""

-- | @a@ element
a_ :: Term arg result => arg -> result
a_ = term "a"

-- | @altglyph@ element
altGlyph_ :: Monad m => [Attribute] -> HtmlT m ()
altGlyph_ = with $ makeElementNoContent "altGlyph"

-- | @altglyphdef@ element
altGlyphDef_ :: Monad m => [Attribute] -> HtmlT m ()
altGlyphDef_ = with $ makeElementNoContent "altGlyphDef" 

-- | @altglyphitem@ element
altGlyphItem_ :: Monad m => [Attribute] -> HtmlT m ()
altGlyphItem_ = with $ makeElementNoContent "altGlyphItem" 

-- | @animate@ element
animate_ :: Monad m => [Attribute] -> HtmlT m ()
animate_ = with $ makeElementNoContent "animate" 

-- | @animatecolor@ element
animateColor_ :: Monad m => [Attribute] -> HtmlT m ()
animateColor_ = with $ makeElementNoContent "animateColor" 

-- | @animatemotion@ element
animateMotion_ :: Monad m => [Attribute] -> HtmlT m ()
animateMotion_ = with $ makeElementNoContent "animateMotion" 

-- | @animatetransform@ element
animateTransform_ :: Monad m => [Attribute] -> HtmlT m ()
animateTransform_ = with $ makeElementNoContent "animateTransform" 

-- | @circle@ element
circle_ :: Monad m => [Attribute] -> HtmlT m ()
circle_ = with $ makeElementNoContent "circle" 

-- | @clippath@ element
clippath_ :: Term arg result => arg -> result
clippath_ = term "clippath"

-- | @colorProfile@ element 
colorProfile_ :: Monad m => [Attribute] -> HtmlT m ()
colorProfile_ = with $ makeElementNoContent "color-profile" 

-- | @cursor@ element
cursor_ :: Monad m => [Attribute] -> HtmlT m ()
cursor_ = with $ makeElementNoContent "cursor" 

-- | @defs@ element
defs_ :: Term arg result => arg -> result
defs_ = term "defs"

-- | @desc@ element
desc_ :: Monad m => [Attribute] -> HtmlT m ()
desc_ = with $ makeElementNoContent "desc" 

-- | @ellipse@ element
ellipse_ :: Monad m => [Attribute] -> HtmlT m ()
ellipse_ = with $ makeElementNoContent "ellipse" 

-- | @feblend@ element
feBlend_ :: Monad m => [Attribute] -> HtmlT m ()
feBlend_ = with $ makeElementNoContent "feBlend" 

-- | @fecolormatrix@ element
feColorMatrix_ :: Monad m => [Attribute] -> HtmlT m ()
feColorMatrix_ = with $ makeElementNoContent "feColorMatrix" 

-- | @fecomponenttransfer@ element
feComponentTransfer_ :: Monad m => [Attribute] -> HtmlT m ()
feComponentTransfer_ = with $ makeElementNoContent "feComponentTransfer" 

-- | @fecomposite@ element
feComposite_ :: Monad m => [Attribute] -> HtmlT m ()
feComposite_ = with $ makeElementNoContent "feComposite" 

-- | @feconvolvematrix@ element
feConvolveMatrix_ :: Monad m => [Attribute] -> HtmlT m ()
feConvolveMatrix_ = with $ makeElementNoContent "feConvolveMatrix" 

-- | @fediffuselighting@ element
feDiffuseLighting_ :: Monad m => [Attribute] -> HtmlT m ()
feDiffuseLighting_ = with $ makeElementNoContent "feDiffuseLighting" 

-- | @fedisplacementmap@ element
feDisplacementMap_ :: Monad m => [Attribute] -> HtmlT m ()
feDisplacementMap_ = with $ makeElementNoContent "feDisplacementMap" 

-- | @fedistantlight@ element
feDistantLight_ :: Monad m => [Attribute] -> HtmlT m ()
feDistantLight_ = with $ makeElementNoContent "feDistantLight" 

-- | @feflood@ element
feFlood_ :: Monad m => [Attribute] -> HtmlT m ()
feFlood_ = with $ makeElementNoContent "feFlood" 

-- | @fefunca@ element
feFuncA_ :: Monad m => [Attribute] -> HtmlT m ()
feFuncA_ = with $ makeElementNoContent "feFuncA" 

-- | @fefuncb@ element
feFuncB_ :: Monad m => [Attribute] -> HtmlT m ()
feFuncB_ = with $ makeElementNoContent "feFuncB" 

-- | @fefuncg@ element
feFuncG_ :: Monad m => [Attribute] -> HtmlT m ()
feFuncG_ = with $ makeElementNoContent "feFuncG" 

-- | @fefuncr@ element
feFuncR_ :: Monad m => [Attribute] -> HtmlT m ()
feFuncR_ = with $ makeElementNoContent "feFuncR" 

-- | @fegaussianblur@ element
feGaussianBlur_ :: Monad m => [Attribute] -> HtmlT m ()
feGaussianBlur_ = with $ makeElementNoContent "feGaussianBlur" 

-- | @feimage@ element
feImage_ :: Monad m => [Attribute] -> HtmlT m ()
feImage_ = with $ makeElementNoContent "feImage" 

-- | @femerge@ element
feMerge_ :: Monad m => [Attribute] -> HtmlT m ()
feMerge_ = with $ makeElementNoContent "feMerge" 

-- | @femergenode@ element
feMergeNode_ :: Monad m => [Attribute] -> HtmlT m ()
feMergeNode_ = with $ makeElementNoContent "feMergeNode" 

-- | @femorphology@ element
feMorphology_ :: Monad m => [Attribute] -> HtmlT m ()
feMorphology_ = with $ makeElementNoContent "feMorphology" 

-- | @feoffset@ element
feOffset_ :: Monad m => [Attribute] -> HtmlT m ()
feOffset_ = with $ makeElementNoContent "feOffset" 

-- | @fepointlight@ element
fePointLight_ :: Monad m => [Attribute] -> HtmlT m ()
fePointLight_ = with $ makeElementNoContent "fePointLight" 

-- | @fespecularlighting@ element
feSpecularLighting_ :: Monad m => [Attribute] -> HtmlT m ()
feSpecularLighting_ = with $ makeElementNoContent "feSpecularLighting" 

-- | @fespotlight@ element
feSpotLight_ :: Monad m => [Attribute] -> HtmlT m ()
feSpotLight_ = with $ makeElementNoContent "feSpotLight" 

-- | @fetile@ element
feTile_ :: Monad m => [Attribute] -> HtmlT m ()
feTile_ = with $ makeElementNoContent "feTile" 

-- | @feturbulence@ element
feTurbulence_ :: Monad m => [Attribute] -> HtmlT m ()
feTurbulence_ = with $ makeElementNoContent "feTurbulence" 

-- | @filter_@ element
filter_ :: Monad m => [Attribute] -> HtmlT m ()
filter_ = with $ makeElementNoContent "filter" 

-- | @font@ element
font_ :: Monad m => [Attribute] -> HtmlT m ()
font_ = with $ makeElementNoContent "font" 

-- | @fontFace@ element
fontFace_ :: Monad m => [Attribute] -> HtmlT m ()
fontFace_ = with $ makeElementNoContent "font-face" 

-- | @fontFaceFormat@ element
fontFaceFormat_ :: Monad m => [Attribute] -> HtmlT m ()
fontFaceFormat_ = with $ makeElementNoContent "font-face-format" 

-- | @fontFaceName@ element
fontFaceName_ :: Monad m => [Attribute] -> HtmlT m ()
fontFaceName_ = with $ makeElementNoContent "font-face-name" 

-- | @fontFaceSrc@ element
fontFaceSrc_ :: Monad m => [Attribute] -> HtmlT m ()
fontFaceSrc_ = with $ makeElementNoContent "font-face-src" 

-- | @fontFaceUri@ element
fontFaceUri_ :: Monad m => [Attribute] -> HtmlT m ()
fontFaceUri_ = with $ makeElementNoContent "font-face-uri" 

-- | @foreignobject@ element
foreignObject_ :: Monad m => [Attribute] -> HtmlT m ()
foreignObject_ = with $ makeElementNoContent "foreignObject" 

-- | @g@ element
g_ :: Term arg result => arg -> result
g_ = term "g"

-- | @glyph@ element or attribute
glyph_ :: Term arg result => arg -> result
glyph_ = term "glyph"

-- | @glyphref@ element
glyphRef_ :: Monad m => [Attribute] -> HtmlT m ()
glyphRef_ = with $ makeElementNoContent "glyphRef" 

-- | @hkern@ element
hkern_ :: Monad m => [Attribute] -> HtmlT m ()
hkern_ = with $ makeElementNoContent "hkern" 

-- | @image@ element
image_ :: Monad m => [Attribute] -> HtmlT m ()
image_ = with $ makeElementNoContent "image" 

-- | @line@ element
line_ :: Monad m => [Attribute] -> HtmlT m ()
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
metadata_ :: Monad m => [Attribute] -> HtmlT m ()
metadata_ = with $ makeElementNoContent "metadata" 

-- | @missingGlyph@ element
missingGlyph_ :: Term arg result => arg -> result
missingGlyph_ = term "missing-glyph"

-- | @mpath@ element
mpath_ :: Monad m => [Attribute] -> HtmlT m ()
mpath_ = with $ makeElementNoContent "mpath" 

-- | @path@ element
path_ :: Monad m => [Attribute] -> HtmlT m ()
path_ = with $ makeElementNoContent "path" 

-- | @pattern@ element
pattern_ :: Term arg result => arg -> result
pattern_ = term "pattern"

-- | @polygon@ element
polygon_ :: Monad m => [Attribute] -> HtmlT m ()
polygon_ = with $ makeElementNoContent "polygon" 

-- | @polyline@ element
polyline_ :: Monad m => [Attribute] -> HtmlT m ()
polyline_ = with $ makeElementNoContent "polyline" 

-- | @radialgradient@ element
radialGradient_ :: Term arg result => arg -> result
radialGradient_ = term "radialGradient"

-- | @rect@ element
rect_ :: Monad m => [Attribute] -> HtmlT m ()
rect_ = with $ makeElementNoContent "rect"

-- | @script@ element
script_ :: Monad m => [Attribute] -> HtmlT m ()
script_ = with $ makeElementNoContent "script" 

-- | @set@ element
set_ :: Monad m => [Attribute] -> HtmlT m ()
set_ = with $ makeElementNoContent "set" 

-- | @stop@ element
stop_ :: Monad m => [Attribute] -> HtmlT m ()
stop_ = with $ makeElementNoContent "stop" 

-- | @style@ element
style_ :: Monad m => [Attribute] -> HtmlT m ()
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
textPath_ :: Monad m => [Attribute] -> HtmlT m ()
textPath_ = with $ makeElementNoContent "textPath" 

-- | @title@ element
title_ :: Monad m => [Attribute] -> HtmlT m ()
title_ = with $ makeElementNoContent "title" 

-- | @tref@ element
tref_ :: Monad m => [Attribute] -> HtmlT m ()
tref_ = with $ makeElementNoContent "tref" 

-- | @tspan@ element
tspan_ :: Monad m => [Attribute] -> HtmlT m ()
tspan_ = with $ makeElementNoContent "tspan" 

-- | @use@ element
use_ :: Monad m => [Attribute] -> HtmlT m ()
use_ = with $ makeElementNoContent "use" 

-- | @view@ element
view_ :: Monad m => [Attribute] -> HtmlT m ()
view_ = with $ makeElementNoContent "view" 

-- | @vkern@ element
vkern_ :: Monad m => [Attribute] -> HtmlT m ()
vkern_ = with $ makeElementNoContent "vkern" 
