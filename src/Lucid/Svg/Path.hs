{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}

module Lucid.Svg.Path where

import           Data.Text             (Text, pack)
import qualified Data.Text             as T

tt :: Show a => a -> Text
tt = pack . show

-- | moveto (absolute)
mA :: Show a =>  a -> a -> Text
mA x y = T.concat ["M " ,tt x, ",", tt y, " "]

-- | moveto (relative)
mR :: Show a =>  a -> a -> Text
mR dx dy = T.concat ["m ", tt dx, ",", tt dy, " "]

-- | lineto (absolute)
lA :: Show a =>  a -> a -> Text
lA x y = T.concat ["L ", tt x, ",", tt y, " "]

-- | lineto (relative)
lR :: Show a =>  a -> a -> Text
lR dx dy = T.concat ["l ", tt dx, ",", tt dy, " "]

-- | horizontal lineto (absolute)
hA :: Show a =>  a -> Text
hA x = T.concat ["H ", tt x, " "]

-- | horizontal lineto (relative)
hR :: Show a =>  a -> Text
hR dx = T.concat ["h ", tt dx, " "]

-- | vertical lineto (absolute)
vA :: Show a =>  a -> Text
vA y = T.concat ["V ", tt y, " "]

-- | vertical lineto (relative)
vR :: Show a =>  a -> Text
vR dy = T.concat ["v ", tt dy, " "]

-- | Cubic Bezier curve (absolute)
cA :: Show a =>  a -> a -> a -> a -> a -> a -> Text
cA c1x c1y c2x c2y x y = T.concat [ "C ", tt c1x, ",", tt c1y, " ", tt c2x, ","
                                  , tt c2y, " ", tt x, " ", tt y]

-- | Cubic Bezier curve (relative)
cR :: Show a =>  a -> a -> a -> a -> a -> a -> Text
cR dc1x dc1y dc2x dc2y dx dy = T.concat [ "c ", tt dc1x, ",", tt dc1y, " ", tt dc2x
                                        , ",", tt dc2y, " ", tt dx, " ", tt dy]

-- | Smooth Cubic Bezier curve (absolute)
sA :: Show a =>  a -> a -> a -> a -> Text
sA c2x c2y x y = T.concat ["S ", tt c2x, ",", tt c2y, " ", tt x, ",", tt y, " "]

-- | Smooth Cubic Bezier curve (relative)
sR :: Show a =>  a -> a -> a -> a -> Text
sR dc2x dc2y dx dy = T.concat ["s ", tt dc2x, ",", tt dc2y, " ", tt dx, ",", tt dy, " "]

-- | Quadratic Bezier curve (absolute)
qA :: Show a =>  a -> a -> a -> a -> Text
qA cx cy x y = T.concat ["Q ", tt cx, ",", tt cy, " ", tt x, ",", tt y, " "]

-- | Quadratic Bezier curve (relative)
qR :: Show a =>  a -> a -> a -> a -> Text
qR dcx dcy dx dy = T.concat ["q ", tt dcx, ",", tt dcy, " ", tt dx, ",", tt dy, " " ]

-- | Smooth Quadratic Bezier curve (absolute)
tA  :: Show a =>  a -> a -> Text
tA x y = T.concat ["T ", " ", tt x, ",", tt y, " "]

-- | Smooth Quadratic Bezier curve (relative)
tR :: Show a =>  a -> a -> Text
tR x y = T.concat [ "t ", tt x, ",", tt y, " "]

-- | Arc (absolute)
aA :: Show a =>  a -> a -> a -> a -> a -> a -> a -> Text
aA rx ry xrot largeFlag sweepFlag x y =
  T.concat [ "A ", tt rx, ",", tt ry, " ", tt xrot, " ", tt largeFlag
           , " ", tt sweepFlag, " ", tt x, " ", tt y, " "]

-- | Arc (relative)
aR :: Show a =>  a -> a -> a -> a -> a -> a -> a -> Text
aR rx ry xrot largeFlag sweepFlag x y =
  T.concat [ "a ", tt rx, ",", tt ry, " ", tt xrot, " ", tt largeFlag
           , " ", tt sweepFlag, " ", tt x, " ", tt y, " "]

-- | closepath
z :: Text
z = "Z"

-- | SVG Transform components
-- | Specifies a translation by @x@ and @y@
translate :: Show a =>  a -> a -> Text
translate x y = T.concat ["translate(", tt x, " ", tt y, ")"]

-- | Specifies a scale operation by @x@ and @y@
scale :: Show a =>  a -> a -> Text
scale x y = T.concat ["scale(", tt x, " ", tt y, ")"]

-- | Specifies a rotation by @rotate-angle@ degrees
rotate :: Show a =>  a -> Text
rotate angle = T.concat ["rotate(", tt angle, ")"]

-- | Specifies a rotation by @rotate-angle@ degrees about the given time @rx,ry@
rotateAround :: Show a =>  a -> a -> a -> Text
rotateAround angle rx ry = T.concat ["rotate(", tt angle, ",", tt rx, ",", tt ry, ")"]

-- | Skew tansformation along x-axis
skewX :: Show a =>  a -> Text
skewX angle = T.concat ["skewX(", tt angle, ")"]

-- | Skew tansformation along y-axis
skewY :: Show a =>  a -> Text
skewY angle = T.concat ["skewY(", tt angle, ")"]

-- | Specifies a transform in the form of a transformation matrix
matrix :: Show a =>  a -> a -> a -> a -> a -> a -> Text
matrix a b c d e f =  T.concat [ "matrix(", tt a, ",", tt b, ",",  tt c
                               , ",",  tt d, ",", tt e, ",",  tt f, ")"]
