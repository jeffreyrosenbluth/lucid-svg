{-# OPTIONS -fno-warn-unused-imports #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Lucid.Svg
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- DSL for creating SVG.
--
-------------------------------------------------------------------------------
module Lucid.Svg
  ( -- * Intro
    -- $intro
    -- * Types
    Svg
    -- * Re-exports
  , module Lucid.Svg.Path
  , module Lucid.Svg.Elements
  , module Lucid.Svg.Attributes
    -- * From Lucid.Base
  , renderText
  , renderBS
  , renderTextT
  , renderBST
  , renderToFile
    -- ** Running
    -- $running
  , execHtmlT
  , evalHtmlT
  , runHtmlT
    -- ** Types
  , Attribute
    -- ** Classes
    -- $overloaded
  , Term(..)
  , ToHtml(..)
  , With(..)
  ) where

import Data.Functor.Identity
import Lucid.Base
import qualified Lucid.Svg.Attributes as A
import Lucid.Svg.Attributes hiding (colorProfile_, cursor_, filter_, path_, style_)
import Lucid.Svg.Elements
import Lucid.Svg.Path

type Svg = SvgT Identity

-- $intro
--
-- SVG elements and attributes in Lucid-Svg are written with a postfix ‘@_@’. 
-- Some examples:
--
-- 'path_', 'circle_', 'color_', 'scale_'
--
-- Note: If you're testing in the REPL you need to add a type annotation to
-- indicate that you want SVG. In normal code your top-level
-- declaration signatures handle that.
--
-- Plain text is written using the @OverloadedStrings@ and
-- @ExtendedDefaultRules@ extensions, and is automatically escaped: 
--
-- As in Lucid, elements nest by function application:
--
-- >>> g_ (text_ "Hello SVG") :: Svg ()
-- <g><text>Hello SVG</text></g>
--
-- and elements are juxtaposed via monoidal append or monadic sequencing:
--
-- >>> text_ "Hello" <> text_ "SVG" :: Svg ()
-- <text>Hello</text><text>SVG</text>
--
-- >>> do text_ "Hello"; text_ "SVG" :: Svg ()
-- <text>Hello</text><text>SVG</text>
--
-- Attributes are set by providing an argument list. In contrast to HTML
-- many SVG elements have no content, only attributes.
--
-- >>> rect_ [width_ "100%", height_ "100%", fill_ "red"] :: Svg ()
-- <rect height="100%" width="100%" fill="red"></rect>
--
-- Attributes and elements that share the same name are not conflicting
-- unless they appear on the list in the note below:
--
-- >>> mask_ [mask_ "attribute"] "element" :: Svg ()
-- <mask mask="attribute">element</mask>
--
-- Note: The following element and attribute names overlap and cannot be
-- handled polymorphically since doing so would create conflicting functional
-- dependencies. The unqualifed name refers to the element.
-- We qualify the attribute name as @A@. For example, 'path_' and 'A.path_'.
--            
-- 'colorProfile_', 'cursor_', 'filter_', 'path_', and 'style_'
--
-- Path data can be constructed using the functions in 'Lucid.Svg.Path'
-- and combined monoidally:
--
-- @
-- path_ (
--   [ d_ (mA "10" "80" <> qA "52.5" "10" "95" "80" <> tA "180" "80" <> z)
--   , stroke_ "blue"
--   , fill_ "orange"
--   ])
-- @
-- > <path d="M 10,80 Q 52.5,10 95,80 T 180,80 Z" stroke="blue" fill="orange"></path>
--
-- __A slightly longer example__:
--  
-- > import Lucid.Svg
-- > 
-- > svg :: Svg () -> Svg ()
-- > svg content = do
-- >   doctype_
-- >   with (svg11_ content) [version_ "1.1", width_ "300" , height_ "200"]
-- > 
-- > contents :: Svg ()
-- > contents = do
-- >   rect_ [width_ "100%", height_ "100%", fill_ "red"]
-- >   circle_ [cx_ "150", cy_ "100", r_ "80", fill_ "green"]
-- >   text_ [x_ "150", y_ "125", fontSize_ "60", textAnchor_ "middle", fill_ "white"] "SVG"
-- > 
-- > 
-- > main :: IO ()
-- > main = do
-- >   print $ svg contents
-- <<http://i.imgur.com/dXu84xR.png>>
