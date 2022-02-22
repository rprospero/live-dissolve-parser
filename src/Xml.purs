module Xml where

import Prelude
import Data.Array as A
import Data.Foldable (null)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data XmlNode
  = XmlNode String (M.Map String String) (Array XmlNode)

derive instance genericXmlNode :: Generic XmlNode _

instance showXmlNode :: Show XmlNode where
  show x = genericShow x

xmlEmptyNode :: String -> XmlNode
xmlEmptyNode name = XmlNode name M.empty []

class ToAttr a where
  toAttr :: a -> String

instance toAttrString :: ToAttr String where
  toAttr = identity

instance toAttrNumber :: ToAttr Number where
  toAttr = show

instance toAttrInt :: ToAttr Int where
  toAttr = show

instance toArrtBoolean :: ToAttr Boolean where
  toAttr = show

attr :: forall a. ToAttr a => String -> a -> (XmlNode -> XmlNode)
attr key value (XmlNode name as cs) = XmlNode name (M.insert key (toAttr value) as) cs

infix 5 attr as ::=

mayAttr :: forall a. ToAttr a => String -> Maybe a -> (XmlNode -> XmlNode)
mayAttr key Nothing s = s

mayAttr key (Just value) (XmlNode name as cs) = XmlNode name (M.insert key (toAttr value) as) cs

infix 5 mayAttr as ::=?

addChild :: XmlNode -> XmlNode -> XmlNode
addChild child (XmlNode name as cs) = XmlNode name as (cs `A.snoc` child)

infix 5 addChild as ::=>

class ToXml a where
  toXml :: a -> XmlNode

xmlEncode :: XmlNode -> String
xmlEncode (XmlNode name as []) = "<" <> name <> " " <> encodeAttrs as <> " />"

xmlEncode (XmlNode name as children) = "<" <> name <> encodeAttrs as <> ">" <> A.fold (map xmlEncode children) <> "</" <> name <> ">"

encodeAttrs :: M.Map String String -> String
encodeAttrs as = if null as then "" else foldlWithIndex go "" as
  where
  go k s v = s <> " " <> k <> "=\"" <> v <> "\""
