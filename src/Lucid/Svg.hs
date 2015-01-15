{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-type-defaults    #-}

module Lucid.Svg where

import           Lucid.Base
import           Lucid.Svg.Elements

import           Data.Functor.Identity
import           Data.Text             (Text)
import qualified Data.Text             as T

type Svg = HtmlT Identity

-- | @DOCTYPE@ element
doctype_ :: Monad m => HtmlT m ()
doctype_ = makeElementNoEnd "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" 

-- | @svg@ element + svg 1.1 attributes
svg11_:: Term [Attribute] (s -> t) => s -> t
svg11_ m = svg_ [ makeAttribute "xmlns" "http://www.w3.org/2000/svg"
                , makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"]
           m

-- | SVG Text components
--   moveto (absolute)
mA :: Text -> Text -> Text
mA x y = T.concat ["M " ,x, ",", y, " "]

-- | moveto (relative)
mR :: Text -> Text -> Text
mR dx dy = T.concat ["m ", dx, ",", dy, " "]

-- | lineto (absolute)
lA :: Text -> Text -> Text
lA x y = T.concat ["L ", x, ",", y, " "]

-- | lineto (relative)
lR :: Text -> Text -> Text
lR dx dy = T.concat ["l ", dx, ",", dy, " "]

-- | horizontal lineto (absolute)
hA :: Text -> Text
hA x = T.concat ["H ", x, " "]

-- | horizontal lineto (relative)
hR :: Text -> Text
hR dx = T.concat ["h ", dx, " "]

-- | vertical lineto (absolute)
vA :: Text -> Text
vA y = T.concat ["V ", y, " "]

-- | vertical lineto (relative)
vR :: Text -> Text
vR dy = T.concat ["v ", dy, " "]

-- | Cubic Bezier curve (absolute)
cA :: Text -> Text -> Text -> Text -> Text -> Text -> Text
cA c1x c1y c2x c2y x y = T.concat ["C ", c1x, ",", c1y, " ", c2x, ",", c2y, " ", x, " ", y]

-- | Cubic Bezier curve (relative)
cR :: Text -> Text -> Text -> Text -> Text -> Text -> Text
cR dc1x dc1y dc2x dc2y dx dy = T.concat ["c ", dc1x, ",", dc1y, " ", dc2x, ",", dc2y, " ", dx, " ", dy]

-- | Smooth Cubic Bezier curve (absolute)
sA :: Text -> Text -> Text -> Text -> Text
sA c2x c2y x y = T.concat ["S ", c2x, ",", c2y, " ", x, ",", y, " "]

-- | Smooth Cubic Bezier curve (relative)
sR :: Text -> Text -> Text -> Text -> Text
sR dc2x dc2y dx dy = T.concat ["s ", dc2x, ",", dc2y, " ", dx, ",", dy, " "]

-- | Quadratic Bezier curve (absolute)
qA :: Text -> Text -> Text -> Text -> Text
qA cx cy x y = T.concat ["Q ", cx, ",", cy, " ", x, ",", y, " "]

-- | Quadratic Bezier curve (relative)
qR :: Text -> Text -> Text -> Text -> Text
qR dcx dcy dx dy = T.concat ["q ", dcx, ",", dcy, " ", dx, ",", dy, " " ]

-- | Smooth Quadratic Bezier curve (abslute)
tA  :: Text -> Text -> Text
tA x y = T.concat ["T ", " ", x, ",", y, " "]

-- | Smooth Quadratic Bezier curve (relative)
tR :: Text -> Text -> Text
tR x y = T.concat [ "t ", " ", x, ",", y, " "]

-- | SVG Transform components
-- | Specifies a translation by @x@ and @y@
translate :: Text -> Text -> Text
translate x y = T.concat ["translate(", x, " ", y, ")"]

-- | Specifies a scale operation by @x@ and @y@
scale :: Text -> Text -> Text
scale x y = T.concat ["scale(", x, " ", y, ")"]

-- | Specifies a rotation by @rotate-angle@ degrees
rotate :: Text -> Text
rotate angle = T.concat ["rotate(", angle, ")"]

-- | Specifies a rotation by @rotate-angle@ degrees about the given time @rx,ry@
rotateAround :: Text -> Text -> Text -> Text
rotateAround angle rx ry = T.concat ["rotate(", angle, ",", rx, ",", ry, ")"]

-- | Skew tansformation along x-axis
skewX :: Text -> Text
skewX angle = T.concat ["skewX(", angle, ")"]

-- | Skew tansformation along y-axis
skewY :: Text -> Text
skewY angle = T.concat ["skewY(", angle, ")"]

-- | Specifies a transform in the form of a transformation matrix
matrix :: Text -> Text -> Text -> Text -> Text -> Text -> Text
matrix a b c_ d e f =  T.concat ["matrix(",  a, ",",  b, ",",  c_, ",",  d, ",", e, ",",  f, ")"]
