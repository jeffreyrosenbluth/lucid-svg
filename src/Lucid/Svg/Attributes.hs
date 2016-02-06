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

module Lucid.Svg.Attributes
  ( AttrTag(..)
  , bindAttr
  , (->>)
  , (<<-)
  ) where

import Lucid.Svg.Core
import Data.Text (Text)

-- | Make an 'Attribute' from it's value constructor and it's text value.
bindAttr :: AttrTag -> Text -> Attribute
bindAttr t v = makeAttribute (tag2text t) v

-- | Infix version of 'bindAttr'
infixl 4  <<-
(<<-) :: AttrTag -> Text -> Attribute
(<<-) = bindAttr

-- | Infix version of 'bindAttr' with it's arguments reversed.
infixl 4  ->>
(->>) :: Text -> AttrTag -> Attribute
(->>) = flip bindAttr

data AttrTag
  = Accent_height
  | Accumulate
  | Additive
  | Alignment_baseline
  | Alphabetic
  | Amplitude
  | Arabic_form
  | Ascent
  | AttributeName
  | AttributeType
  | Azimuth
  | BaseFrequency
  | Baseprofile
  | Baseline_shift
  | Bbox
  | Begin
  | Bias
  | By
  | CalcMode
  | Cap_height
  | Class
  | Clip
  | Clip_path
  | Clip_rule
  | ClipPathUnits
  | Color
  | Color_interpolation
  | Color_interpolation_filters
  | Color_profile
  | Color_rendering
  | ContentScriptType
  | ContentStyleType
  | Cursor
  | Cx
  | Cy
  | D
  | Descent
  | DiffuseConstant
  | Direction
  | Display
  | Divisor
  | Dominant_baseline
  | Dur
  | Dx
  | Dy
  | EdgeMode
  | Elevation
  | Enable_background
  | End
  | Exponent
  | ExternalResourcesRequired
  | Fill
  | Fill_opacity
  | Fill_rule
  | Filter
  | FilterRes
  | FilterUnits
  | Flood_color
  | Flood_opacity
  | Font_family
  | Font_size
  | Font_size_adjust
  | Font_stretch
  | Font_style
  | Font_variant
  | Font_weight
  | Format
  | From
  | Fx
  | Fy
  | G1
  | G2
  | Glyph_name
  | Glyph_orientation_horizontal
  | Glyph_orientation_vertical
  | GradientTransform
  | GradientUnits
  | Hanging
  | Height
  | Horiz_adv_x
  | Horiz_origin_x
  | Horiz_origin_y
  | Id
  | Ideographic
  | Image_rendering
  | In
  | In2
  | Intercept
  | K
  | K1
  | K2
  | K3
  | K4
  | KernelMatrix
  | KernelUnitLength
  | Kerning
  | KeyPoints
  | KeySplines
  | KeyTimes
  | Lang
  | LengthAdjust
  | Letter_spacing
  | Lighting_color
  | LimitingConeAngle
  | Local
  | Marker_end
  | Marker_mid
  | Marker_start
  | MarkerHeight
  | MarkerUnits
  | MarkerWidth
  | MaskContentUnits
  | MaskUnits
  | Mathematical
  | Max
  | Media
  | Method
  | Min
  | Mode
  | Name
  | NumOctaves
  | Offset
  | Onabort
  | Onactivate
  | Onbegin
  | Onclick
  | Onend
  | Onerror
  | Onfocusin
  | Onfocusout
  | Onload
  | Onmousedown
  | Onmousemove
  | Onmouseout
  | Onmouseover
  | Onmouseup
  | Onrepeat
  | Onresize
  | Onscroll
  | Onunload
  | Onzoom
  | Opacity
  | Operator
  | Order
  | Orient
  | Orientation
  | Origin
  | Overflow
  | Overline_position
  | Overline_thickness
  | Panose_1
  | Paint_order
  | Path
  | PathLength
  | PatternContentUnits
  | PatternTransform
  | PatternUnits
  | Pointer_events
  | Points
  | PointsAtX
  | PointsAtY
  | PointsAtZ
  | PreserveAlpha
  | PreserveAspectRatio
  | PrimitiveUnits
  | R
  | Radius
  | RefX
  | RefY
  | Rendering_intent
  | RepeatCount
  | RepeatDur
  | RequiredExtensions
  | RequiredFeatures
  | Restart
  | Result
  | Rotate
  | Rx
  | Ry
  | Scale
  | Seed
  | Shape_rendering
  | Slope
  | Spacing
  | SpecularConstant
  | SpecularExponent
  | SpreadMethod
  | StartOffset
  | StdDeviation
  | Stemh
  | Stemv
  | StitchTiles
  | Stop_color
  | Stop_opacity
  | Strikethrough_position
  | Strikethrough_thickness
  | String
  | Stroke
  | Stroke_dasharray
  | Stroke_dashoffset
  | Stroke_linecap
  | Stroke_linejoin
  | Stroke_miterlimit
  | Stroke_opacity
  | Stroke_width
  | Style
  | SurfaceScale
  | SystemLanguage
  | TableValues
  | Target
  | TargetX
  | TargetY
  | Text_anchor
  | Text_decoration
  | Text_rendering
  | TextLength
  | To
  | Transform
  | Type
  | U1
  | U2
  | Underline_position
  | Underline_thickness
  | Unicode
  | Unicode_bidi
  | Unicode_range
  | Units_per_em
  | V_alphabetic
  | V_hanging
  | V_ideographic
  | V_mathematical
  | Values
  | Version
  | Vert_adv_y
  | Vert_origin_x
  | Vert_origin_y
  | ViewBox
  | ViewTarget
  | Visibility
  | Width
  | Widths
  | Word_spacing
  | Writing_mode
  | X
  | X_height
  | X1
  | X2
  | XChannelSelector
  | XlinkActuate
  | XlinkArcrole
  | XlinkHref
  | XlinkRole
  | XlinkShow
  | XlinkTitle
  | XlinkType
  | XmlBase
  | XmlLang
  | XmlSpace
  | Y
  | Y1
  | Y2
  | YChannelselector
  | Z
  | ZoomAndPan

-- Link the tags to their svg strings.
tag2text :: AttrTag -> Text
tag2text Accent_height = "accent-height"
tag2text Accumulate = "accumulate"
tag2text Additive = "additive"
tag2text Alignment_baseline = "alignment-baseline"
tag2text Alphabetic = "alphabetic"
tag2text Amplitude = "amplitude"
tag2text Arabic_form = "arabic-form"
tag2text Ascent = "ascent"
tag2text AttributeName = "attributeName"
tag2text AttributeType = "attributeType"
tag2text Azimuth = "azimuth"
tag2text BaseFrequency = "baseFrequency"
tag2text Baseprofile = "baseprofile"
tag2text Baseline_shift = "baseline-shift"
tag2text Bbox = "bbox"
tag2text Begin = "begin"
tag2text Bias = "bias"
tag2text By = "by"
tag2text CalcMode = "calcMode"
tag2text Cap_height = "cap-height"
tag2text Class = "class"
tag2text Clip = "clip"
tag2text Clip_path = "clip-path"
tag2text Clip_rule = "clip-rule"
tag2text ClipPathUnits = "clipPathUnits"
tag2text Color = "color"
tag2text Color_interpolation = "color-interpolation"
tag2text Color_interpolation_filters = "color-interpolation-filters"
tag2text Color_profile = "color-profile"
tag2text Color_rendering = "color-rendering"
tag2text ContentScriptType = "contentScriptType"
tag2text ContentStyleType = "contentStyleType"
tag2text Cursor = "cursor"
tag2text Cx = "cx"
tag2text Cy = "cy"
tag2text D = "d"
tag2text Descent = "descent"
tag2text DiffuseConstant = "diffuseConstant"
tag2text Direction = "direction"
tag2text Display = "display"
tag2text Divisor = "divisor"
tag2text Dominant_baseline = "dominant-baseline"
tag2text Dur = "dur"
tag2text Dx = "dx"
tag2text Dy = "dy"
tag2text EdgeMode = "edgeMode"
tag2text Elevation = "elevation"
tag2text Enable_background = "enable-background"
tag2text End = "end"
tag2text Exponent = "exponent"
tag2text ExternalResourcesRequired = "externalResourcesRequired"
tag2text Fill = "fill"
tag2text Fill_opacity = "fill-opacity"
tag2text Fill_rule = "fill-rule"
tag2text Filter = "filter"
tag2text FilterRes = "filterRes"
tag2text FilterUnits = "filterUnits"
tag2text Flood_color = "flood-color"
tag2text Flood_opacity = "flood-opacity"
tag2text Font_family = "font-family"
tag2text Font_size = "font-size"
tag2text Font_size_adjust = "font-size-adjust"
tag2text Font_stretch = "font-stretch"
tag2text Font_style = "font-style"
tag2text Font_variant = "font-variant"
tag2text Font_weight = "font-weight"
tag2text Format = "format"
tag2text From = "from"
tag2text Fx = "fx"
tag2text Fy = "fy"
tag2text G1 = "g1"
tag2text G2 = "g2"
tag2text Glyph_name = "glyph-name"
tag2text Glyph_orientation_horizontal = "glyph-orientation-horizontal"
tag2text Glyph_orientation_vertical = "glyph-orientation-vertical"
tag2text GradientTransform = "gradientTransform"
tag2text GradientUnits = "gradientUnits"
tag2text Hanging = "hanging"
tag2text Height = "height"
tag2text Horiz_adv_x = "horiz-adv-x"
tag2text Horiz_origin_x = "horiz-origin-x"
tag2text Horiz_origin_y = "horiz-origin-y"
tag2text Id = "id"
tag2text Ideographic = "ideographic"
tag2text Image_rendering = "image-rendering"
tag2text In = "in"
tag2text In2 = "in2"
tag2text Intercept = "intercept"
tag2text K = "k"
tag2text K1 = "k1"
tag2text K2 = "k2"
tag2text K3 = "k3"
tag2text K4 = "k4"
tag2text KernelMatrix = "kernelMatrix"
tag2text KernelUnitLength = "kernelUnitLength"
tag2text Kerning = "kerning"
tag2text KeyPoints = "keyPoints"
tag2text KeySplines = "keySplines"
tag2text KeyTimes = "keyTimes"
tag2text Lang = "lang"
tag2text LengthAdjust = "lengthAdjust"
tag2text Letter_spacing = "letter-spacing"
tag2text Lighting_color = "lighting-color"
tag2text LimitingConeAngle = "limitingConeAngle"
tag2text Local = "local"
tag2text Marker_end = "marker-end"
tag2text Marker_mid = "marker-mid"
tag2text Marker_start = "marker-start"
tag2text MarkerHeight = "markerHeight"
tag2text MarkerUnits = "markerUnits"
tag2text MarkerWidth = "markerWidth"
tag2text MaskContentUnits = "maskContentUnits"
tag2text MaskUnits = "maskUnits"
tag2text Mathematical = "mathematical"
tag2text Max = "max"
tag2text Media = "media"
tag2text Method = "method"
tag2text Min = "min"
tag2text Mode = "mode"
tag2text Name = "name"
tag2text NumOctaves = "numOctaves"
tag2text Offset = "offset"
tag2text Onabort = "onabort"
tag2text Onactivate = "onactivate"
tag2text Onbegin = "onbegin"
tag2text Onclick = "onclick"
tag2text Onend = "onend"
tag2text Onerror = "onerror"
tag2text Onfocusin = "onfocusin"
tag2text Onfocusout = "onfocusout"
tag2text Onload = "onload"
tag2text Onmousedown = "onmousedown"
tag2text Onmousemove = "onmousemove"
tag2text Onmouseout = "onmouseout"
tag2text Onmouseover = "onmouseover"
tag2text Onmouseup = "onmouseup"
tag2text Onrepeat = "onrepeat"
tag2text Onresize = "onresize"
tag2text Onscroll = "onscroll"
tag2text Onunload = "onunload"
tag2text Onzoom = "onzoom"
tag2text Opacity = "opacity"
tag2text Operator = "operator"
tag2text Order = "order"
tag2text Orient = "orient"
tag2text Orientation = "orientation"
tag2text Origin = "origin"
tag2text Overflow = "overflow"
tag2text Overline_position = "overline-position"
tag2text Overline_thickness = "overline-thickness"
tag2text Panose_1 = "panose-1"
tag2text Paint_order = "paint-order"
tag2text Path = "path"
tag2text PathLength = "pathLength"
tag2text PatternContentUnits = "patternContentUnits"
tag2text PatternTransform = "patternTransform"
tag2text PatternUnits = "patternUnits"
tag2text Pointer_events = "pointer-events"
tag2text Points = "points"
tag2text PointsAtX = "pointsAtX"
tag2text PointsAtY = "pointsAtY"
tag2text PointsAtZ = "pointsAtZ"
tag2text PreserveAlpha = "preserveAlpha"
tag2text PreserveAspectRatio = "preserveAspectRatio"
tag2text PrimitiveUnits = "primitiveUnits"
tag2text R = "r"
tag2text Radius = "radius"
tag2text RefX = "refX"
tag2text RefY = "refY"
tag2text Rendering_intent = "rendering-intent"
tag2text RepeatCount = "repeatCount"
tag2text RepeatDur = "repeatDur"
tag2text RequiredExtensions = "requiredExtensions"
tag2text RequiredFeatures = "requiredFeatures"
tag2text Restart = "restart"
tag2text Result = "result"
tag2text Rotate = "rotate"
tag2text Rx = "rx"
tag2text Ry = "ry"
tag2text Scale = "scale"
tag2text Seed = "seed"
tag2text Shape_rendering = "shape-rendering"
tag2text Slope = "slope"
tag2text Spacing = "spacing"
tag2text SpecularConstant = "specularConstant"
tag2text SpecularExponent = "specularExponent"
tag2text SpreadMethod = "spreadMethod"
tag2text StartOffset = "startOffset"
tag2text StdDeviation = "stdDeviation"
tag2text Stemh = "stemh"
tag2text Stemv = "stemv"
tag2text StitchTiles = "stitchTiles"
tag2text Stop_color = "stop-color"
tag2text Stop_opacity = "stop-opacity"
tag2text Strikethrough_position = "strikethrough-position"
tag2text Strikethrough_thickness = "strikethrough-thickness"
tag2text String = "string"
tag2text Stroke = "stroke"
tag2text Stroke_dasharray = "stroke-dasharray"
tag2text Stroke_dashoffset = "stroke-dashoffset"
tag2text Stroke_linecap = "stroke-linecap"
tag2text Stroke_linejoin = "stroke-linejoin"
tag2text Stroke_miterlimit = "stroke-miterlimit"
tag2text Stroke_opacity = "stroke-opacity"
tag2text Stroke_width = "stroke-width"
tag2text Style = "style"
tag2text SurfaceScale = "surfaceScale"
tag2text SystemLanguage = "systemLanguage"
tag2text TableValues = "tableValues"
tag2text Target = "target"
tag2text TargetX = "targetX"
tag2text TargetY = "targetY"
tag2text Text_anchor = "text-anchor"
tag2text Text_decoration = "text-decoration"
tag2text Text_rendering = "text-rendering"
tag2text TextLength = "textLength"
tag2text To = "to"
tag2text Transform = "transform"
tag2text Type = "type"
tag2text U1 = "u1"
tag2text U2 = "u2"
tag2text Underline_position = "underline-position"
tag2text Underline_thickness = "underline-thickness"
tag2text Unicode = "unicode"
tag2text Unicode_bidi = "unicode-bidi"
tag2text Unicode_range = "unicode-range"
tag2text Units_per_em = "units-per-em"
tag2text V_alphabetic = "v-alphabetic"
tag2text V_hanging = "v-hanging"
tag2text V_ideographic = "v-ideographic"
tag2text V_mathematical = "v-mathematical"
tag2text Values = "values"
tag2text Version = "version"
tag2text Vert_adv_y = "vert-adv-y"
tag2text Vert_origin_x = "vert-origin-x"
tag2text Vert_origin_y = "vert-origin-y"
tag2text ViewBox = "viewBox"
tag2text ViewTarget = "viewTarget"
tag2text Visibility = "visibility"
tag2text Width = "width"
tag2text Widths = "widths"
tag2text Word_spacing = "word-spacing"
tag2text Writing_mode = "writing-mode"
tag2text X = "x"
tag2text X_height = "x-height"
tag2text X1 = "x1"
tag2text X2 = "x2"
tag2text XChannelSelector = "xChannelSelector"
tag2text XlinkActuate = "xlink:actuate"
tag2text XlinkArcrole = "xlink:arcrole"
tag2text XlinkHref = "xlink:href"
tag2text XlinkRole = "xlink:role"
tag2text XlinkShow = "xlink:show"
tag2text XlinkTitle = "xlink:title"
tag2text XlinkType = "xlink:type"
tag2text XmlBase = "xml:base"
tag2text XmlLang = "xml:lang"
tag2text XmlSpace = "xml:space"
tag2text Y = "y"
tag2text Y1 = "y1"
tag2text Y2 = "y2"
tag2text YChannelselector = "yChannelSelector"
tag2text Z = "z"
tag2text ZoomAndPan = "zoomAndPan"
