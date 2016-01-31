{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  SVG.Core
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
--
-- SVG elements.
--
-------------------------------------------------------------------------------

module Lucid.Svg.Core
( -- * Types
  Attribute(..)
, Element
, ToElement(..)
  -- * Combinators
, makeAttribute
, makeElement
, makeElementNoEnd
, makeXmlElementNoEnd
, element
, with
, nil
  -- * Rendering
, renderBS
, renderToFile
, renderText
) where

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Html.Utf8 as BB
import qualified Data.ByteString.Lazy as LB
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe (fromMaybe)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

--------------------------------------------------------------------------------
-- Types

-- | Attribute name value.
data Attribute = Attribute !Text !Text
  deriving (Show,Eq)

-- | Type of an SVG element.
type Element = HashMap Text Text -> Builder

instance IsString Element where
  fromString = toElement

-- | Things that can be converted to SVG elements.
class ToElement a where
  toElement :: a -> Element

instance ToElement String where
  toElement = const . BB.fromHtmlEscapedString

instance ToElement Text where
  toElement = const . BB.fromHtmlEscapedText

instance ToElement LT.Text where
  toElement = const . BB.fromHtmlEscapedLazyText

--------------------------------------------------------------------------------
-- Combinators

-- | Make an attribute.
makeAttribute :: Text -- ^ Attribute name.
              -> Text -- ^ Attribute value.
              -> Attribute
makeAttribute x y = Attribute x y

-- | Union two sets of attributes and append duplicate keys.
unionAttrs :: HashMap Text Text -> HashMap Text Text -> HashMap Text Text
unionAttrs = M.unionWith (<>)

-- | Add a list of attributes to an element
with :: Element -> [Attribute] -> Element
with ml attrs attrs' = ml (unionAttrs (M.fromListWith (<>) (map toPair attrs)) attrs')
  where
    toPair (Attribute x y) = (x,y)

-- | Used to make specific SVG element builders.
element :: Text -> [Attribute] -> Element -> Element
element name attrs ml = with (makeElement name ml) attrs

-- | The empty element.
nil :: Element
nil = mempty

-- | Make an SVG element builder
makeElement :: Text -> Element -> Element
makeElement name children attrs =
     s2b "<" <> BB.fromText name
  <> foldlMapWithKey buildAttr attrs <> s2b ">"
  <> children mempty
  <> s2b "</" <> BB.fromText name <> s2b ">"

-- | Make an SVG element builder with no end tag.
makeElementNoEnd :: Text -> Element
makeElementNoEnd name attrs =
     s2b "<" <> BB.fromText name
  <> foldlMapWithKey buildAttr attrs <> s2b ">"

-- | Make an XML element with no end tag.
makeXmlElementNoEnd :: Text -> Element
makeXmlElementNoEnd name attrs =
     s2b "<" <> BB.fromText name
  <> foldlMapWithKey buildAttr attrs <> s2b "/>"

-- | Folding and monoidally appending attributes.
foldlMapWithKey :: Monoid m => (k -> v -> m) -> HashMap k v -> m
foldlMapWithKey f = M.foldlWithKey' (\m k v -> m <> f k v) mempty

s2b :: String -> Builder
s2b = BB.fromString

-- | Build and encode an attribute.
buildAttr :: Text -> Text -> Builder
buildAttr key val =
  s2b " " <>
  BB.fromText key <>
  if val == mempty
    then mempty
    else s2b "=\"" <> BB.fromHtmlEscapedText val <> s2b "\""

--------------------------------------------------------------------------------
-- Rendering

-- | Render a 'Element' to lazy bytestring.
renderBS :: Element -> ByteString
renderBS ml = BB.toLazyByteString $ ml mempty

-- | Render a 'Element' to a file.
renderToFile :: FilePath -> Element -> IO ()
renderToFile fp = LB.writeFile fp . renderBS

-- | Reder an 'Element' to lazy text.
renderText :: Element -> LT.Text
renderText = LT.decodeUtf8 . renderBS
