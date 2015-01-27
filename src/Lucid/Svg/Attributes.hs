{-# LANGUAGE OverloadedStrings #-}
 
-------------------------------------------------------------------------------
-- |
-- Module      :  Lucid.Svg.Attributes
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- SVG Attributes.
--
-------------------------------------------------------------------------------

module Lucid.Svg.Attributes where 

import Lucid.Base
import Data.Text (Text)

-- | The @accentHeight@ attribute.
accentHeight_ :: Text -> Attribute
accentHeight_ = makeAttribute "accent-height"

-- | The @accumulate@ attribute.
accumulate_ :: Text -> Attribute
accumulate_ = makeAttribute "accumulate"

-- | The @additive@ attribute.
additive_ :: Text -> Attribute
additive_ = makeAttribute "additive"

-- | The @alignmentBaseline@ attribute.
alignmentBaseline_ :: Text -> Attribute
alignmentBaseline_ = makeAttribute "alignment-baseline"

-- | The @alphabetic@ attribute.
alphabetic_ :: Text -> Attribute
alphabetic_ = makeAttribute "alphabetic"

-- | The @amplitude@ attribute.
amplitude_ :: Text -> Attribute
amplitude_ = makeAttribute "amplitude"

-- | The @arabicForm@ attribute.
arabicForm_ :: Text -> Attribute
arabicForm_ = makeAttribute "arabic-form"

-- | The @ascent@ attribute.
ascent_ :: Text -> Attribute
ascent_ = makeAttribute "ascent"

-- | The @attributename@ attribute.
attributeName_ :: Text -> Attribute
attributeName_ = makeAttribute "attributeName"

-- | The @attributetype@ attribute.
attributeType_ :: Text -> Attribute
attributeType_ = makeAttribute "attributeType"

-- | The @azimuth@ attribute.
azimuth_ :: Text -> Attribute
azimuth_ = makeAttribute "azimuth"

-- | The @basefrequency@ attribute.
baseFrequency_ :: Text -> Attribute
baseFrequency_ = makeAttribute "baseFrequency"

-- | The @baseprofile@ attribute.
baseprofile_ :: Text -> Attribute
baseprofile_ = makeAttribute "baseprofile"

-- | The @baselineShift@ attribute.
baseline_shift_ :: Text -> Attribute
baseline_shift_ = makeAttribute "baseline-shift"

-- | The @bbox@ attribute.
bbox_ :: Text -> Attribute
bbox_ = makeAttribute "bbox"

-- | The @begin@ attribute.
begin_ :: Text -> Attribute
begin_ = makeAttribute "begin"

-- | The @bias@ attribute.
bias_ :: Text -> Attribute
bias_ = makeAttribute "bias"

-- | The @by@ attribute.
by_ :: Text -> Attribute
by_ = makeAttribute "by"

-- | The @calcmode@ attribute.
calcMode_ :: Text -> Attribute
calcMode_ = makeAttribute "calcMode"

-- | The @capHeight@ attribute.
cap_height_ :: Text -> Attribute
cap_height_ = makeAttribute "cap-height"

-- | The @class@ attribute.
class_ :: Text -> Attribute
class_ = makeAttribute "class"

-- | The @clip@ attribute.
clip_ :: Text -> Attribute
clip_ = makeAttribute "clip"

-- | The @clip-path@ attribute.
clip_path_ :: Text -> Attribute
clip_path_ = makeAttribute "clip-path"

-- | The @clipRule@ attribute.
clip_rule_ :: Text -> Attribute
clip_rule_ = makeAttribute "clip-rule"

-- | The @clippathunits@ attribute.
clipPathUnits_ :: Text -> Attribute
clipPathUnits_ = makeAttribute "clipPathUnits"

-- | The @color@ attribute.
color_ :: Text -> Attribute
color_ = makeAttribute "color"

-- | The @colorInterpolation@ attribute.
color_interpolation_ :: Text -> Attribute
color_interpolation_ = makeAttribute "color-interpolation"

-- | The @colorInterpolationFilters@ attribute.
color_interpolation_filters_ :: Text -> Attribute
color_interpolation_filters_ = makeAttribute "color-interpolation-filters"

-- | The @colorProfile@ attribute.
color_profile_ :: Text -> Attribute
color_profile_ = makeAttribute "color-profile"

-- | The @colorRendering@ attribute.
color_rendering_ :: Text -> Attribute
color_rendering_ = makeAttribute "color-rendering"

-- | The @contentscripttype@ attribute.
contentScriptType_ :: Text -> Attribute
contentScriptType_ = makeAttribute "contentScriptType"

-- | The @contentstyletype@ attribute.
contentStyleType_ :: Text -> Attribute
contentStyleType_ = makeAttribute "contentStyleType"

-- | The @cursor@ attribute.
cursor_ :: Text -> Attribute
cursor_ = makeAttribute "cursor"

-- | The @cx@ attribute.
cx_ :: Text -> Attribute
cx_ = makeAttribute "cx"

-- | The @cy@ attribute.
cy_ :: Text -> Attribute
cy_ = makeAttribute "cy"

-- | The @d@ attribute.
d_ :: Text -> Attribute
d_ = makeAttribute "d"

-- | The @descent@ attribute.
descent_ :: Text -> Attribute
descent_ = makeAttribute "descent"

-- | The @diffuseconstant@ attribute.
diffuseConstant_ :: Text -> Attribute
diffuseConstant_ = makeAttribute "diffuseConstant"

-- | The @direction@ attribute.
direction_ :: Text -> Attribute
direction_ = makeAttribute "direction"

-- | The @display@ attribute.
display_ :: Text -> Attribute
display_ = makeAttribute "display"

-- | The @divisor@ attribute.
divisor_ :: Text -> Attribute
divisor_ = makeAttribute "divisor"

-- | The @dominantBaseline@ attribute.
dominant_baseline_ :: Text -> Attribute
dominant_baseline_ = makeAttribute "dominant-baseline"

-- | The @dur@ attribute.
dur_ :: Text -> Attribute
dur_ = makeAttribute "dur"

-- | The @dx@ attribute.
dx_ :: Text -> Attribute
dx_ = makeAttribute "dx"

-- | The @dy@ attribute.
dy_ :: Text -> Attribute
dy_ = makeAttribute "dy"

-- | The @edgemode@ attribute.
edgeMode_ :: Text -> Attribute
edgeMode_ = makeAttribute "edgeMode"

-- | The @elevation@ attribute.
elevation_ :: Text -> Attribute
elevation_ = makeAttribute "elevation"

-- | The @enableBackground@ attribute.
enable_background_ :: Text -> Attribute
enable_background_ = makeAttribute "enable-background"

-- | The @end@ attribute.
end_ :: Text -> Attribute
end_ = makeAttribute "end"

-- | The @exponent@ attribute.
exponent_ :: Text -> Attribute
exponent_ = makeAttribute "exponent"

-- | The @externalresourcesrequired@ attribute.
externalResourcesRequired_ :: Text -> Attribute
externalResourcesRequired_ = makeAttribute "externalResourcesRequired"

-- | The @fill@ attribute.
fill_ :: Text -> Attribute
fill_ = makeAttribute "fill"

-- | The @fillOpacity@ attribute.
fill_opacity_ :: Text -> Attribute
fill_opacity_ = makeAttribute "fill-opacity"

-- | The @fillRule@ attribute.
fill_rule_ :: Text -> Attribute
fill_rule_ = makeAttribute "fill-rule"

-- | The @filter@ attribute.
filter_ :: Text -> Attribute
filter_ = makeAttribute "filter"

-- | The @filterres@ attribute.
filterRes_ :: Text -> Attribute
filterRes_ = makeAttribute "filterRes"

-- | The @filterunits@ attribute.
filterUnits_ :: Text -> Attribute
filterUnits_ = makeAttribute "filterUnits"

-- | The @floodColor@ attribute.
flood_color_ :: Text -> Attribute
flood_color_ = makeAttribute "flood-color"

-- | The @floodOpacity@ attribute.
flood_opacity_ :: Text -> Attribute
flood_opacity_ = makeAttribute "flood-opacity"

-- | The @fontFamily@ attribute.
font_family_ :: Text -> Attribute
font_family_ = makeAttribute "font-family"

-- | The @fontSize@ attribute.
font_size_ :: Text -> Attribute
font_size_ = makeAttribute "font-size"

-- | The @fontSizeAdjust@ attribute.
font_size_adjust_ :: Text -> Attribute
font_size_adjust_ = makeAttribute "font-size-adjust"

-- | The @fontStretch@ attribute.
font_stretch_ :: Text -> Attribute
font_stretch_ = makeAttribute "font-stretch"

-- | The @fontStyle@ attribute.
font_style_ :: Text -> Attribute
font_style_ = makeAttribute "font-style"

-- | The @fontVariant@ attribute.
font_variant_ :: Text -> Attribute
font_variant_ = makeAttribute "font-variant"

-- | The @fontWeight@ attribute.
font_weight_ :: Text -> Attribute
font_weight_ = makeAttribute "font-weight"

-- | The @format@ attribute.
format_ :: Text -> Attribute
format_ = makeAttribute "format"

-- | The @from@ attribute.
from_ :: Text -> Attribute
from_ = makeAttribute "from"

-- | The @fx@ attribute.
fx_ :: Text -> Attribute
fx_ = makeAttribute "fx"

-- | The @fy@ attribute.
fy_ :: Text -> Attribute
fy_ = makeAttribute "fy"

-- | The @g1@ attribute.
g1_ :: Text -> Attribute
g1_ = makeAttribute "g1"

-- | The @g2@ attribute.
g2_ :: Text -> Attribute
g2_ = makeAttribute "g2"

-- | The @glyphName@ attribute.
glyph_name_ :: Text -> Attribute
glyph_name_ = makeAttribute "glyph-name"

-- | The @glyphOrientationHorizontal@ attribute.
glyph_orientation_horizontal_ :: Text -> Attribute
glyph_orientation_horizontal_ = makeAttribute "glyph-orientation-horizontal"

-- | The @glyphOrientationVertical@ attribute.
glyph_orientation_vertical_ :: Text -> Attribute
glyph_orientation_vertical_ = makeAttribute "glyph-orientation-vertical"

-- | The @-- | The @gradienttransform@ attribute.
gradientTransform_ :: Text -> Attribute
gradientTransform_ = makeAttribute "gradientTransform"

-- | The @gradientunits@ attribute.
gradientUnits_ :: Text -> Attribute
gradientUnits_ = makeAttribute "gradientUnits"

-- | The @hanging@ attribute.
hanging_ :: Text -> Attribute
hanging_ = makeAttribute "hanging"

-- | The @height@ attribute.
height_ :: Text -> Attribute
height_ = makeAttribute "height"

-- | The @horizAdvX@ attribute.
horiz_adv_x_ :: Text -> Attribute
horiz_adv_x_ = makeAttribute "horiz-adv-x"

-- | The @horizOriginX@ attribute.
horiz_origin_x_ :: Text -> Attribute
horiz_origin_x_ = makeAttribute "horiz-origin-x"

-- | The @horizOriginY@ attribute.
horiz_origin_y_ :: Text -> Attribute
horiz_origin_y_ = makeAttribute "horiz-origin-y"

-- | The @id@ attribute.
id_ :: Text -> Attribute
id_ = makeAttribute "id"

-- | The @ideographic@ attribute.
ideographic_ :: Text -> Attribute
ideographic_ = makeAttribute "ideographic"

-- | The @imageRendering@ attribute.
image_rendering_ :: Text -> Attribute
image_rendering_ = makeAttribute "image-rendering"

-- | The @in@ attribute.
in_ :: Text -> Attribute
in_ = makeAttribute "in"

-- | The @in2@ attribute.
in2_ :: Text -> Attribute
in2_ = makeAttribute "in2"

-- | The @intercept@ attribute.
intercept_ :: Text -> Attribute
intercept_ = makeAttribute "intercept"

-- | The @k@ attribute.
k_ :: Text -> Attribute
k_ = makeAttribute "k"

-- | The @k1@ attribute.
k1_ :: Text -> Attribute
k1_ = makeAttribute "k1"

-- | The @k2@ attribute.
k2_ :: Text -> Attribute
k2_ = makeAttribute "k2"

-- | The @k3@ attribute.
k3_ :: Text -> Attribute
k3_ = makeAttribute "k3"

-- | The @k4@ attribute.
k4_ :: Text -> Attribute
k4_ = makeAttribute "k4"

-- | The @kernelmatrix@ attribute.
kernelMatrix_ :: Text -> Attribute
kernelMatrix_ = makeAttribute "kernelMatrix"

-- | The @kernelunitlength@ attribute.
kernelUnitLength_ :: Text -> Attribute
kernelUnitLength_ = makeAttribute "kernelUnitLength"

-- | The @kerning@ attribute.
kerning_ :: Text -> Attribute
kerning_ = makeAttribute "kerning"

-- | The @keypoints@ attribute.
keyPoints_ :: Text -> Attribute
keyPoints_ = makeAttribute "keyPoints"

-- | The @keysplines@ attribute.
keySplines_ :: Text -> Attribute
keySplines_ = makeAttribute "keySplines"

-- | The @keytimes@ attribute.
keyTimes_ :: Text -> Attribute
keyTimes_ = makeAttribute "keyTimes"

-- | The @lang@ attribute.
lang_ :: Text -> Attribute
lang_ = makeAttribute "lang"

-- | The @lengthadjust@ attribute.
lengthAdjust_ :: Text -> Attribute
lengthAdjust_ = makeAttribute "lengthAdjust"

-- | The @letterSpacing@ attribute.
letter_spacing_ :: Text -> Attribute
letter_spacing_ = makeAttribute "letter-spacing"

-- | The @lightingColor@ attribute.
lighting_color_ :: Text -> Attribute
lighting_color_ = makeAttribute "lighting-color"

-- | The @limitingconeangle@ attribute.
limitingConeAngle_ :: Text -> Attribute
limitingConeAngle_ = makeAttribute "limitingConeAngle"

-- | The @local@ attribute.
local_ :: Text -> Attribute
local_ = makeAttribute "local"

-- | The @markerEnd@ attribute.
marker_end_ :: Text -> Attribute
marker_end_ = makeAttribute "marker-end"

-- | The @markerMid@ attribute.
marker_mid_ :: Text -> Attribute
marker_mid_ = makeAttribute "marker-mid"

-- | The @markerStart@ attribute.
marker_start_ :: Text -> Attribute
marker_start_ = makeAttribute "marker-start"

-- | The @markerheight@ attribute.
markerHeight_ :: Text -> Attribute
markerHeight_ = makeAttribute "markerHeight"

-- | The @markerunits@ attribute.
markerUnits_ :: Text -> Attribute
markerUnits_ = makeAttribute "markerUnits"

-- | The @markerwidth@ attribute.
markerWidth_ :: Text -> Attribute
markerWidth_ = makeAttribute "markerWidth"

-- | The @maskcontentunits@ attribute.
maskContentUnits_ :: Text -> Attribute
maskContentUnits_ = makeAttribute "maskContentUnits"

-- | The @maskunits@ attribute.
maskUnits_ :: Text -> Attribute
maskUnits_ = makeAttribute "maskUnits"

-- | The @mathematical@ attribute.
mathematical_ :: Text -> Attribute
mathematical_ = makeAttribute "mathematical"

-- | The @max@ attribute.
max_ :: Text -> Attribute
max_ = makeAttribute "max"

-- | The @media@ attribute.
media_ :: Text -> Attribute
media_ = makeAttribute "media"

-- | The @method@ attribute.
method_ :: Text -> Attribute
method_ = makeAttribute "method"

-- | The @min@ attribute.
min_ :: Text -> Attribute
min_ = makeAttribute "min"

-- | The @mode@ attribute.
mode_ :: Text -> Attribute
mode_ = makeAttribute "mode"

-- | The @name@ attribute.
name_ :: Text -> Attribute
name_ = makeAttribute "name"

-- | The @numoctaves@ attribute.
numOctaves_ :: Text -> Attribute
numOctaves_ = makeAttribute "numOctaves"

-- | The @offset@ attribute.
offset_ :: Text -> Attribute
offset_ = makeAttribute "offset"

-- | The @onabort@ attribute.
onabort_ :: Text -> Attribute
onabort_ = makeAttribute "onabort"

-- | The @onactivate@ attribute.
onactivate_ :: Text -> Attribute
onactivate_ = makeAttribute "onactivate"

-- | The @onbegin@ attribute.
onbegin_ :: Text -> Attribute
onbegin_ = makeAttribute "onbegin"

-- | The @onclick@ attribute.
onclick_ :: Text -> Attribute
onclick_ = makeAttribute "onclick"

-- | The @onend@ attribute.
onend_ :: Text -> Attribute
onend_ = makeAttribute "onend"

-- | The @onerror@ attribute.
onerror_ :: Text -> Attribute
onerror_ = makeAttribute "onerror"

-- | The @onfocusin@ attribute.
onfocusin_ :: Text -> Attribute
onfocusin_ = makeAttribute "onfocusin"

-- | The @onfocusout@ attribute.
onfocusout_ :: Text -> Attribute
onfocusout_ = makeAttribute "onfocusout"

-- | The @onload@ attribute.
onload_ :: Text -> Attribute
onload_ = makeAttribute "onload"

-- | The @onmousedown@ attribute.
onmousedown_ :: Text -> Attribute
onmousedown_ = makeAttribute "onmousedown"

-- | The @onmousemove@ attribute.
onmousemove_ :: Text -> Attribute
onmousemove_ = makeAttribute "onmousemove"

-- | The @onmouseout@ attribute.
onmouseout_ :: Text -> Attribute
onmouseout_ = makeAttribute "onmouseout"

-- | The @onmouseover@ attribute.
onmouseover_ :: Text -> Attribute
onmouseover_ = makeAttribute "onmouseover"

-- | The @onmouseup@ attribute.
onmouseup_ :: Text -> Attribute
onmouseup_ = makeAttribute "onmouseup"

-- | The @onrepeat@ attribute.
onrepeat_ :: Text -> Attribute
onrepeat_ = makeAttribute "onrepeat"

-- | The @onresize@ attribute.
onresize_ :: Text -> Attribute
onresize_ = makeAttribute "onresize"

-- | The @onscroll@ attribute.
onscroll_ :: Text -> Attribute
onscroll_ = makeAttribute "onscroll"

-- | The @onunload@ attribute.
onunload_ :: Text -> Attribute
onunload_ = makeAttribute "onunload"

-- | The @onzoom@ attribute.
onzoom_ :: Text -> Attribute
onzoom_ = makeAttribute "onzoom"

-- | The @opacity@ attribute.
opacity_ :: Text -> Attribute
opacity_ = makeAttribute "opacity"

-- | The @operator@ attribute.
operator_ :: Text -> Attribute
operator_ = makeAttribute "operator"

-- | The @order@ attribute.
order_ :: Text -> Attribute
order_ = makeAttribute "order"

-- | The @orient@ attribute.
orient_ :: Text -> Attribute
orient_ = makeAttribute "orient"

-- | The @orientation@ attribute.
orientation_ :: Text -> Attribute
orientation_ = makeAttribute "orientation"

-- | The @origin@ attribute.
origin_ :: Text -> Attribute
origin_ = makeAttribute "origin"

-- | The @overflow@ attribute.
overflow_ :: Text -> Attribute
overflow_ = makeAttribute "overflow"

-- | The @overlinePosition@ attribute.
overline_position_ :: Text -> Attribute
overline_position_ = makeAttribute "overline-position"

-- | The @overlineThickness@ attribute.
overline_thickness_ :: Text -> Attribute
overline_thickness_ = makeAttribute "overline-thickness"

-- | The @panose1@ attribute.
panose_1_ :: Text -> Attribute
panose_1_ = makeAttribute "panose-1"

-- | The @paint-order@ attribute.
paint_order_ :: Text -> Attribute
paint_order_ = makeAttribute "paint-order"

-- | The @path@ attribute.
path_ :: Text -> Attribute
path_ = makeAttribute "path"

-- | The @pathlength@ attribute.
pathLength_ :: Text -> Attribute
pathLength_ = makeAttribute "pathLength"

-- | The @patterncontentunits@ attribute.
patternContentUnits_ :: Text -> Attribute
patternContentUnits_ = makeAttribute "patternContentUnits"

-- | The @patterntransform@ attribute.
patternTransform_ :: Text -> Attribute
patternTransform_ = makeAttribute "patternTransform"

-- | The @patternunits@ attribute.
patternUnits_ :: Text -> Attribute
patternUnits_ = makeAttribute "patternUnits"

-- | The @pointerEvents@ attribute.
pointer_events_ :: Text -> Attribute
pointer_events_ = makeAttribute "pointer-events"

-- | The @points@ attribute.
points_ :: Text -> Attribute
points_ = makeAttribute "points"

-- | The @pointsatx@ attribute.
pointsAtX_ :: Text -> Attribute
pointsAtX_ = makeAttribute "pointsAtX"

-- | The @pointsaty@ attribute.
pointsAtY_ :: Text -> Attribute
pointsAtY_ = makeAttribute "pointsAtY"

-- | The @pointsatz@ attribute.
pointsAtZ_ :: Text -> Attribute
pointsAtZ_ = makeAttribute "pointsAtZ"

-- | The @preservealpha@ attribute.
preserveAlpha_ :: Text -> Attribute
preserveAlpha_ = makeAttribute "preserveAlpha"

-- | The @preserveaspectratio@ attribute.
preserveAspectRatio_ :: Text -> Attribute
preserveAspectRatio_ = makeAttribute "preserveAspectRatio"

-- | The @primitiveunits@ attribute.
primitiveUnits_ :: Text -> Attribute
primitiveUnits_ = makeAttribute "primitiveUnits"

-- | The @r@ attribute.
r_ :: Text -> Attribute
r_ = makeAttribute "r"

-- | The @radius@ attribute.
radius_ :: Text -> Attribute
radius_ = makeAttribute "radius"

-- | The @refx@ attribute.
refX_ :: Text -> Attribute
refX_ = makeAttribute "refX"

-- | The @refy@ attribute.
refY_ :: Text -> Attribute
refY_ = makeAttribute "refY"

-- | The @renderingIntent@ attribute.
rendering_intent_ :: Text -> Attribute
rendering_intent_ = makeAttribute "rendering-intent"

-- | The @repeatcount@ attribute.
repeatCount_ :: Text -> Attribute
repeatCount_ = makeAttribute "repeatCount"

-- | The @repeatdur@ attribute.
repeatDur_ :: Text -> Attribute
repeatDur_ = makeAttribute "repeatDur"

-- | The @requiredextensions@ attribute.
requiredExtensions_ :: Text -> Attribute
requiredExtensions_ = makeAttribute "requiredExtensions"

-- | The @requiredfeatures@ attribute.
requiredFeatures_ :: Text -> Attribute
requiredFeatures_ = makeAttribute "requiredFeatures"

-- | The @restart@ attribute.
restart_ :: Text -> Attribute
restart_ = makeAttribute "restart"

-- | The @result@ attribute.
result_ :: Text -> Attribute
result_ = makeAttribute "result"

-- | The @rotate@ attribute.
rotate_ :: Text -> Attribute
rotate_ = makeAttribute "rotate"

-- | The @rx@ attribute.
rx_ :: Text -> Attribute
rx_ = makeAttribute "rx"

-- | The @ry@ attribute.
ry_ :: Text -> Attribute
ry_ = makeAttribute "ry"

-- | The @scale@ attribute.
scale_ :: Text -> Attribute
scale_ = makeAttribute "scale"

-- | The @seed@ attribute.
seed_ :: Text -> Attribute
seed_ = makeAttribute "seed"

-- | The @shapeRendering@ attribute.
shape_rendering_ :: Text -> Attribute
shape_rendering_ = makeAttribute "shape-rendering"

-- | The @slope@ attribute.
slope_ :: Text -> Attribute
slope_ = makeAttribute "slope"

-- | The @spacing@ attribute.
spacing_ :: Text -> Attribute
spacing_ = makeAttribute "spacing"

-- | The @specularconstant@ attribute.
specularConstant_ :: Text -> Attribute
specularConstant_ = makeAttribute "specularConstant"

-- | The @specularexponent@ attribute.
specularExponent_ :: Text -> Attribute
specularExponent_ = makeAttribute "specularExponent"

-- | The @spreadmethod@ attribute.
spreadMethod_ :: Text -> Attribute
spreadMethod_ = makeAttribute "spreadMethod"

-- | The @startoffset@ attribute.
startOffset_ :: Text -> Attribute
startOffset_ = makeAttribute "startOffset"

-- | The @stddeviation@ attribute.
stdDeviation_ :: Text -> Attribute
stdDeviation_ = makeAttribute "stdDeviation"

-- | The @stemh@ attribute.
stemh_ :: Text -> Attribute
stemh_ = makeAttribute "stemh"

-- | The @stemv@ attribute.
stemv_ :: Text -> Attribute
stemv_ = makeAttribute "stemv"

-- | The @stitchtiles@ attribute.
stitchTiles_ :: Text -> Attribute
stitchTiles_ = makeAttribute "stitchTiles"

-- | The @stopColor@ attribute.
stop_color_ :: Text -> Attribute
stop_color_ = makeAttribute "stop-color"

-- | The @stopOpacity@ attribute.
stop_opacity_ :: Text -> Attribute
stop_opacity_ = makeAttribute "stop-opacity"

-- | The @strikethroughPosition@ attribute.
strikethrough_position_ :: Text -> Attribute
strikethrough_position_ = makeAttribute "strikethrough-position"

-- | The @strikethroughThickness@ attribute.
strikethrough_thickness_ :: Text -> Attribute
strikethrough_thickness_ = makeAttribute "strikethrough-thickness"

-- | The @string@ attribute.
string_ :: Text -> Attribute
string_ = makeAttribute "string"

-- | The @stroke@ attribute.
stroke_ :: Text -> Attribute
stroke_ = makeAttribute "stroke"

-- | The @strokeDasharray@ attribute.
stroke_dasharray_ :: Text -> Attribute
stroke_dasharray_ = makeAttribute "stroke-dasharray"

-- | The @strokeDashoffset@ attribute.
stroke_dashoffset_ :: Text -> Attribute
stroke_dashoffset_ = makeAttribute "stroke-dashoffset"

-- | The @strokeLinecap@ attribute.
stroke_linecap_ :: Text -> Attribute
stroke_linecap_ = makeAttribute "stroke-linecap"

-- | The @strokeLinejoin@ attribute.
stroke_linejoin_ :: Text -> Attribute
stroke_linejoin_ = makeAttribute "stroke-linejoin"

-- | The @strokeMiterlimit@ attribute.
stroke_miterlimit_ :: Text -> Attribute
stroke_miterlimit_ = makeAttribute "stroke-miterlimit"

-- | The @strokeOpacity@ attribute.
stroke_opacity_ :: Text -> Attribute
stroke_opacity_ = makeAttribute "stroke-opacity"

-- | The @strokeWidth@ attribute.
stroke_width_ :: Text -> Attribute
stroke_width_ = makeAttribute "stroke-width"

-- | The @style@ attribute.
style_ :: Text -> Attribute
style_ = makeAttribute "style"

-- | The @surfacescale@ attribute.
surfaceScale_ :: Text -> Attribute
surfaceScale_ = makeAttribute "surfaceScale"

-- | The @systemlanguage@ attribute.
systemLanguage_ :: Text -> Attribute
systemLanguage_ = makeAttribute "systemLanguage"

-- | The @tablevalues@ attribute.
tableValues_ :: Text -> Attribute
tableValues_ = makeAttribute "tableValues"

-- | The @target@ attribute.
target_ :: Text -> Attribute
target_ = makeAttribute "target"

-- | The @targetx@ attribute.
targetX_ :: Text -> Attribute
targetX_ = makeAttribute "targetX"

-- | The @targety@ attribute.
targetY_ :: Text -> Attribute
targetY_ = makeAttribute "targetY"

-- | The @textAnchor@ attribute.
text_anchor_ :: Text -> Attribute
text_anchor_ = makeAttribute "text-anchor"

-- | The @textDecoration@ attribute.
text_decoration_ :: Text -> Attribute
text_decoration_ = makeAttribute "text-decoration"

-- | The @textRendering@ attribute.
text_rendering_ :: Text -> Attribute
text_rendering_ = makeAttribute "text-rendering"

-- | The @textlength@ attribute.
textLength_ :: Text -> Attribute
textLength_ = makeAttribute "textLength"

-- | The @to@ attribute.
to_ :: Text -> Attribute
to_ = makeAttribute "to"

-- | The @transform@ attribute.
transform_ :: Text -> Attribute
transform_ = makeAttribute "transform"

-- | The @type@ attribute.
type_ :: Text -> Attribute
type_ = makeAttribute "type"

-- | The @u1@ attribute.
u1_ :: Text -> Attribute
u1_ = makeAttribute "u1"

-- | The @u2@ attribute.
u2_ :: Text -> Attribute
u2_ = makeAttribute "u2"

-- | The @underlinePosition@ attribute.
underline_position_ :: Text -> Attribute
underline_position_ = makeAttribute "underline-position"

-- | The @underlineThickness@ attribute.
underline_thickness_ :: Text -> Attribute
underline_thickness_ = makeAttribute "underline-thickness"

-- | The @unicode@ attribute.
unicode_ :: Text -> Attribute
unicode_ = makeAttribute "unicode"

-- | The @unicodeBidi@ attribute.
unicode_bidi_ :: Text -> Attribute
unicode_bidi_ = makeAttribute "unicode-bidi"

-- | The @unicodeRange@ attribute.
unicode_range_ :: Text -> Attribute
unicode_range_ = makeAttribute "unicode-range"

-- | The @unitsPerEm@ attribute.
units_per_em_ :: Text -> Attribute
units_per_em_ = makeAttribute "units-per-em"

-- | The @vAlphabetic@ attribute.
v_alphabetic_ :: Text -> Attribute
v_alphabetic_ = makeAttribute "v-alphabetic"

-- | The @vHanging@ attribute.
v_hanging_ :: Text -> Attribute
v_hanging_ = makeAttribute "v-hanging"

-- | The @vIdeographic@ attribute.
v_ideographic_ :: Text -> Attribute
v_ideographic_ = makeAttribute "v-ideographic"

-- | The @vMathematical@ attribute.
v_mathematical_ :: Text -> Attribute
v_mathematical_ = makeAttribute "v-mathematical"

-- | The @values@ attribute.
values_ :: Text -> Attribute
values_ = makeAttribute "values"

-- | The @version@ attribute.
version_ :: Text -> Attribute
version_ = makeAttribute "version"

-- | The @vertAdvY@ attribute.
vert_adv_y_ :: Text -> Attribute
vert_adv_y_ = makeAttribute "vert-adv-y"

-- | The @vertOriginX@ attribute.
vert_origin_x_ :: Text -> Attribute
vert_origin_x_ = makeAttribute "vert-origin-x"

-- | The @vertOriginY@ attribute.
vert_origin_y_ :: Text -> Attribute
vert_origin_y_ = makeAttribute "vert-origin-y"

-- | The @viewbox@ attribute.
viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

-- | The @viewtarget@ attribute.
viewTarget_ :: Text -> Attribute
viewTarget_ = makeAttribute "viewTarget"

-- | The @visibility@ attribute.
visibility_ :: Text -> Attribute
visibility_ = makeAttribute "visibility"

-- | The @width@ attribute.
width_ :: Text -> Attribute
width_ = makeAttribute "width"

-- | The @widths@ attribute.
widths_ :: Text -> Attribute
widths_ = makeAttribute "widths"

-- | The @wordSpacing@ attribute.
word_spacing_ :: Text -> Attribute
word_spacing_ = makeAttribute "word-spacing"

-- | The @writingMode@ attribute.
writing_mode_ :: Text -> Attribute
writing_mode_ = makeAttribute "writing-mode"

-- | The @x@ attribute.
x_ :: Text -> Attribute
x_ = makeAttribute "x"

-- | The @xHeight@ attribute.
x_height_ :: Text -> Attribute
x_height_ = makeAttribute "x-height"

-- | The @x1@ attribute.
x1_ :: Text -> Attribute
x1_ = makeAttribute "x1"

-- | The @x2@ attribute.
x2_ :: Text -> Attribute
x2_ = makeAttribute "x2"

-- | The @xchannelselector@ attribute.
xChannelSelector_ :: Text -> Attribute
xChannelSelector_ = makeAttribute "xChannelSelector"

-- | The @xlinkActuate@ attribute.
xlinkActuate_ :: Text -> Attribute
xlinkActuate_ = makeAttribute "xlink:actuate"

-- | The @xlinkArcrole@ attribute.
xlinkArcrole_ :: Text -> Attribute
xlinkArcrole_ = makeAttribute "xlink:arcrole"

-- | The @xlinkHref@ attribute.
xlinkHref_ :: Text -> Attribute
xlinkHref_ = makeAttribute "xlink:href"

-- | The @xlinkRole@ attribute.
xlinkRole_ :: Text -> Attribute
xlinkRole_ = makeAttribute "xlink:role"

-- | The @xlinkShow@ attribute.
xlinkShow_ :: Text -> Attribute
xlinkShow_ = makeAttribute "xlink:show"

-- | The @xlinkTitle@ attribute.
xlinkTitle_ :: Text -> Attribute
xlinkTitle_ = makeAttribute "xlink:title"

-- | The @xlinkType@ attribute.
xlinkType_ :: Text -> Attribute
xlinkType_ = makeAttribute "xlink:type"

-- | The @xmlBase@ attribute.
xmlBase_ :: Text -> Attribute
xmlBase_ = makeAttribute "xml:base"

-- | The @xmlLang@ attribute.
xmlLang_ :: Text -> Attribute
xmlLang_ = makeAttribute "xml:lang"

-- | The @xmlSpace@ attribute.
xmlSpace_ :: Text -> Attribute
xmlSpace_ = makeAttribute "xml:space"

-- | The @y@ attribute.
y_ :: Text -> Attribute
y_ = makeAttribute "y"

-- | The @y1@ attribute.
y1_ :: Text -> Attribute
y1_ = makeAttribute "y1"

-- | The @y2@ attribute.
y2_ :: Text -> Attribute
y2_ = makeAttribute "y2"

-- | The @ychannelselector@ attribute.
yChannelselector_ :: Text -> Attribute
yChannelselector_ = makeAttribute "yChannelSelector"

-- | The @z@ attribute.
z_ :: Text -> Attribute
z_ = makeAttribute "z"

-- | The @zoomandpan@ attribute.
zoomAndPan_ :: Text -> Attribute
zoomAndPan_ = makeAttribute "zoomAndPan"
