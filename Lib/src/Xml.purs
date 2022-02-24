module Xml where

import Data.Lens
import Prelude
import Control.Monad.State (class MonadState, State, execState, get, modify, put)
import Data.Array as A
import Data.Foldable (null)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Zoom (zoom)
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

xmlName :: Lens' XmlNode String
xmlName = lens (\(XmlNode n _ _) -> n) (\(XmlNode _ m xs) n -> XmlNode n m xs)

xmlAttr :: Lens' XmlNode (M.Map String String)
xmlAttr = lens (\(XmlNode _ m _) -> m) (\(XmlNode n _ xs) m -> XmlNode n m xs)

xmlChildren :: Lens' XmlNode (Array XmlNode)
xmlChildren = lens (\(XmlNode _ _ xs) -> xs) (\(XmlNode n m _) xs -> XmlNode n m xs)

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
attr key value = set (xmlAttr <<< at key) $ pure (toAttr value)

infix 5 attr as ::=

mayAttr :: forall a. ToAttr a => String -> Maybe a -> (XmlNode -> XmlNode)
mayAttr key Nothing s = s

mayAttr key (Just value) (XmlNode name as cs) = XmlNode name (M.insert key (toAttr value) as) cs

infix 5 mayAttr as ::=?

addChild :: XmlNode -> XmlNode -> XmlNode
addChild child = over xmlChildren (flip A.snoc child) -- XmlNode name as (cs `A.snoc` child)

infix 5 addChild as ::=>

class ToXml a where
  toXml :: a -> XmlNode

fullXmlEncode :: XmlNode -> String
fullXmlEncode x = "<?xml version=\"1.0\"?>\n" <> xmlEncode x

xmlEncode :: XmlNode -> String
xmlEncode (XmlNode name as []) = "<" <> name <> " " <> encodeAttrs as <> " />"

xmlEncode (XmlNode name as children) = "<" <> name <> encodeAttrs as <> ">" <> A.fold (map xmlEncode children) <> "</" <> name <> ">"

encodeAttrs :: M.Map String String -> String
encodeAttrs as = if null as then "" else foldlWithIndex go "" as
  where
  go k s v = s <> " " <> k <> "=\"" <> v <> "\""

addTerms :: forall a. ToAttr a => String -> Array a -> XmlNode
addTerms name = A.foldl go (xmlEmptyNode name)
  where
  go s x = (("value" ::= x) $ xmlEmptyNode "term") ::=> s

xmlActOn :: String -> Array (XmlNode -> XmlNode) -> XmlNode
xmlActOn name fs = A.foldl go (xmlEmptyNode name) fs
  where
  go s f = f s

onNewChild :: String -> State XmlNode Unit -> State XmlNode Unit
onNewChild name act =
  zoom xmlChildren do
    len <- A.length <$> get
    _ <- modify (flip A.snoc $ xmlEmptyNode name) -- xmlActOn "master" (map xmlOnMaster xs))
    zoom (ix len) act

onAttr :: forall m a. MonadState XmlNode m => ToAttr a => String -> a -> m Unit
onAttr name x = (xmlAttr <<< at name) .= pure (toAttr x)

onAttrMay :: forall m a. MonadState XmlNode m => ToAttr a => String -> Maybe a -> m Unit
onAttrMay name Nothing = pure unit

onAttrMay name (Just x) = (xmlAttr <<< at name) .= pure (toAttr x)
