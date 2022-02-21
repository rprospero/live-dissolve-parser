module Xml where

import Prelude
import Data.Map as M
import Data.Array as A
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)

data XmlNode
  = XmlNode String (M.Map String String) (Array XmlNode)

derive instance genericXmlNode :: Generic XmlNode _

instance showXmlNode :: Show XmlNode where
  show x = genericShow x

xmlEmptyNode :: String -> XmlNode
xmlEmptyNode name = XmlNode name M.empty []

attr :: forall a. Show a => String -> a -> (XmlNode -> XmlNode)
attr key value (XmlNode name as cs) = XmlNode name (M.insert key (show value) as) cs

infix 5 attr as ::=

addChild :: XmlNode -> XmlNode -> XmlNode
addChild child (XmlNode name as cs) = XmlNode name as (cs `A.snoc` child)

infix 5 addChild as ::=>

class ToXml a where
  toXml :: a -> XmlNode
