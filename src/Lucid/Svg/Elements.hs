{-# LANGUAGE OverloadedStrings         #-}

module Lucid.Svg.Elements where 

import Lucid.Base

-- | @a@ element
a_ :: Term arg result => arg -> result
a_ = term "a"

-- | @altglyph@ element
altglyph_ :: Term arg result => arg -> result
altglyph_ = term "altglyph"

-- | @altglyphdef@ element
altglyphdef_ :: Term arg result => arg -> result
altglyphdef_ = term "altglyphdef"

-- | @altglyphitem@ element
altglyphitem_ :: Term arg result => arg -> result
altglyphitem_ = term "altglyphitem"

-- | @animate@ element
animate_ :: Term arg result => arg -> result
animate_ = term "animate"

-- | @animatecolor@ element
animatecolor_ :: Term arg result => arg -> result
animatecolor_ = term "animatecolor"

-- | @animatemotion@ element
animatemotion_ :: Term arg result => arg -> result
animatemotion_ = term "animatemotion"

-- | @animatetransform@ element
animatetransform_ :: Term arg result => arg -> result
animatetransform_ = term "animatetransform"

-- | @circle@ element
circle_ :: Term arg result => arg -> result
circle_ = term "circle"

-- | @clippath@ element
clippath_ :: Term arg result => arg -> result
clippath_ = term "clippath"

-- | @colorProfile@ element
colorProfile_ :: Term arg result => arg -> result
colorProfile_ = term "colorProfile"

-- | @cursor@ element
cursor_ :: Term arg result => arg -> result
cursor_ = term "cursor"

-- | @defs@ element
defs_ :: Term arg result => arg -> result
defs_ = term "defs"

-- | @desc@ element
desc_ :: Term arg result => arg -> result
desc_ = term "desc"

-- | @ellipse@ element
ellipse_ :: Term arg result => arg -> result
ellipse_ = term "ellipse"

-- | @feblend@ element
feblend_ :: Term arg result => arg -> result
feblend_ = term "feblend"

-- | @fecolormatrix@ element
fecolormatrix_ :: Term arg result => arg -> result
fecolormatrix_ = term "fecolormatrix"

-- | @fecomponenttransfer@ element
fecomponenttransfer_ :: Term arg result => arg -> result
fecomponenttransfer_ = term "fecomponenttransfer"

-- | @fecomposite@ element
fecomposite_ :: Term arg result => arg -> result
fecomposite_ = term "fecomposite"

-- | @feconvolvematrix@ element
feconvolvematrix_ :: Term arg result => arg -> result
feconvolvematrix_ = term "feconvolvematrix"

-- | @fediffuselighting@ element
fediffuselighting_ :: Term arg result => arg -> result
fediffuselighting_ = term "fediffuselighting"

-- | @fedisplacementmap@ element
fedisplacementmap_ :: Term arg result => arg -> result
fedisplacementmap_ = term "fedisplacementmap"

-- | @fedistantlight@ element
fedistantlight_ :: Term arg result => arg -> result
fedistantlight_ = term "fedistantlight"

-- | @feflood@ element
feflood_ :: Term arg result => arg -> result
feflood_ = term "feflood"

-- | @fefunca@ element
fefunca_ :: Term arg result => arg -> result
fefunca_ = term "fefunca"

-- | @fefuncb@ element
fefuncb_ :: Term arg result => arg -> result
fefuncb_ = term "fefuncb"

-- | @fefuncg@ element
fefuncg_ :: Term arg result => arg -> result
fefuncg_ = term "fefuncg"

-- | @fefuncr@ element
fefuncr_ :: Term arg result => arg -> result
fefuncr_ = term "fefuncr"

-- | @fegaussianblur@ element
fegaussianblur_ :: Term arg result => arg -> result
fegaussianblur_ = term "fegaussianblur"

-- | @feimage@ element
feimage_ :: Term arg result => arg -> result
feimage_ = term "feimage"

-- | @femerge@ element
femerge_ :: Term arg result => arg -> result
femerge_ = term "femerge"

-- | @femergenode@ element
femergenode_ :: Term arg result => arg -> result
femergenode_ = term "femergenode"

-- | @femorphology@ element
femorphology_ :: Term arg result => arg -> result
femorphology_ = term "femorphology"

-- | @feoffset@ element
feoffset_ :: Term arg result => arg -> result
feoffset_ = term "feoffset"

-- | @fepointlight@ element
fepointlight_ :: Term arg result => arg -> result
fepointlight_ = term "fepointlight"

-- | @fespecularlighting@ element
fespecularlighting_ :: Term arg result => arg -> result
fespecularlighting_ = term "fespecularlighting"

-- | @fespotlight@ element
fespotlight_ :: Term arg result => arg -> result
fespotlight_ = term "fespotlight"

-- | @fetile@ element
fetile_ :: Term arg result => arg -> result
fetile_ = term "fetile"

-- | @feturbulence@ element
feturbulence_ :: Term arg result => arg -> result
feturbulence_ = term "feturbulence"

-- | @filter_@ element
filter__ :: Term arg result => arg -> result
filter__ = term "filter_"

-- | @font@ element
font_ :: Term arg result => arg -> result
font_ = term "font"

-- | @fontFace@ element
fontFace_ :: Term arg result => arg -> result
fontFace_ = term "fontFace"

-- | @fontFaceFormat@ element
fontFaceFormat_ :: Term arg result => arg -> result
fontFaceFormat_ = term "fontFaceFormat"

-- | @fontFaceName@ element
fontFaceName_ :: Term arg result => arg -> result
fontFaceName_ = term "fontFaceName"

-- | @fontFaceSrc@ element
fontFaceSrc_ :: Term arg result => arg -> result
fontFaceSrc_ = term "fontFaceSrc"

-- | @fontFaceUri@ element
fontFaceUri_ :: Term arg result => arg -> result
fontFaceUri_ = term "fontFaceUri"

-- | @foreignobject@ element
foreignobject_ :: Term arg result => arg -> result
foreignobject_ = term "foreignobject"

-- | @g@ element
g_ :: Term arg result => arg -> result
g_ = term "g"

-- | @glyph@ element
glyph_ :: Term arg result => arg -> result
glyph_ = term "glyph"

-- | @glyphref@ element
glyphref_ :: Term arg result => arg -> result
glyphref_ = term "glyphref"

-- | @hkern@ element
hkern_ :: Term arg result => arg -> result
hkern_ = term "hkern"

-- | @image@ element
image_ :: Term arg result => arg -> result
image_ = term "image"

-- | @line@ element
line_ :: Term arg result => arg -> result
line_ = term "line"

-- | @lineargradient@ element
lineargradient_ :: Term arg result => arg -> result
lineargradient_ = term "lineargradient"

-- | @marker@ element
marker_ :: Term arg result => arg -> result
marker_ = term "marker"

-- | @mask@ element
mask_ :: Term arg result => arg -> result
mask_ = term "mask"

-- | @metadata@ element
metadata_ :: Term arg result => arg -> result
metadata_ = term "metadata"

-- | @missingGlyph@ element
missingGlyph_ :: Term arg result => arg -> result
missingGlyph_ = term "missingGlyph"

-- | @mpath@ element
mpath_ :: Term arg result => arg -> result
mpath_ = term "mpath"

-- | @path@ element
path_ :: Term arg result => arg -> result
path_ = term "path"

-- | @pattern@ element
pattern_ :: Term arg result => arg -> result
pattern_ = term "pattern"

-- | @polygon@ element
polygon_ :: Term arg result => arg -> result
polygon_ = term "polygon"

-- | @polyline@ element
polyline_ :: Term arg result => arg -> result
polyline_ = term "polyline"

-- | @radialgradient@ element
radialgradient_ :: Term arg result => arg -> result
radialgradient_ = term "radialgradient"

-- | @rect@ element
rect_ :: Term arg result => arg -> result
rect_ = term "rect"

-- | @script@ element
script_ :: Term arg result => arg -> result
script_ = term "script"

-- | @set@ element
set_ :: Term arg result => arg -> result
set_ = term "set"

-- | @stop@ element
stop_ :: Term arg result => arg -> result
stop_ = term "stop"

-- | @style@ element
style_ :: Term arg result => arg -> result
style_ = term "style"

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
text__ :: Term arg result => arg -> result
text__ = term "text_"

-- | @textpath@ element
textpath_ :: Term arg result => arg -> result
textpath_ = term "textpath"

-- | @title@ element
title_ :: Term arg result => arg -> result
title_ = term "title"

-- | @tref@ element
tref_ :: Term arg result => arg -> result
tref_ = term "tref"

-- | @tspan@ element
tspan_ :: Term arg result => arg -> result
tspan_ = term "tspan"

-- | @use@ element
use_ :: Term arg result => arg -> result
use_ = term "use"

-- | @view@ element
view_ :: Term arg result => arg -> result
view_ = term "view"

-- | @vkern@ element
vkern_ :: Term arg result => arg -> result
vkern_ = term "vkern"
