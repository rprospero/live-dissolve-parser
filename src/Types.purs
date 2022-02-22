module Types where

import Configuration
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Foreign.Object
import Layer
import Master
import PairPotential
import Prelude
import Species
import Xml
import Control.Monad.State (execState, modify)
import Data.Array (catMaybes, foldl, head, tail)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

data Section
  = Species String (Array SpeciesPart)
  | Configuration String (Array ConfigurationPart)
  | PairPotentials (Array PairPart)
  | Layer String (Array LayerPart)
  | Master (Array MasterPart)

derive instance genericSection :: Generic Section _

instance showSection :: Show Section where
  show x = genericShow x

data Dissolve
  = Dissolve (Maybe (Array MasterPart)) (Array (Tuple String (Array ConfigurationPart))) (Array (Tuple String (Array LayerPart))) (Maybe (Array PairPart)) (Array (Tuple String (Array SpeciesPart)))

instance encodeDissolve :: EncodeJson Dissolve where
  encodeJson (Dissolve master config layer pair species) =
    flip execState jsonEmptyObject
      $ do
          _ <- modify (writeConfig config)
          _ <- modify (writeLayer layer)
          _ <- modify (writeSpecies species)
          _ <- modify (writePair pair)
          _ <- modify (writeMaster master)
          pure 7

instance toXmlDissolve :: ToXml Dissolve where
  toXml (Dissolve master config layer pair species) =
    flip execState (xmlEmptyNode "dissolve")
      $ do
          _ <- modify (xmlSpecies species)
          _ <- modify (xmlPair pair)
          modify (xmlMaster master)

writeLayer :: (Array (Tuple String (Array LayerPart))) -> Json -> Json
writeLayer xs s = "layer" := foldl go jsonEmptyObject xs ~> s
  where
  go state (Tuple name xs) = name := (popOnLayer xs jsonEmptyObject) ~> state

writeSpecies :: (Array (Tuple String (Array SpeciesPart))) -> Json -> Json
writeSpecies xs s = "species" := foldl go jsonEmptyObject xs ~> s
  where
  go state (Tuple name xs) = name := (popOnSpecies xs jsonEmptyObject) ~> state

xmlSpecies :: (Array (Tuple String (Array SpeciesPart))) -> XmlNode -> XmlNode
xmlSpecies xs s = foldl go s xs
  where
  go state (Tuple name ss) = (foldl (flip xmlOnSpecies) ("name" ::= name $ xmlEmptyNode "species") ss) ::=> state

writeConfig :: (Array (Tuple String (Array ConfigurationPart))) -> Json -> Json
writeConfig xs s = "configuration" := foldl go jsonEmptyObject xs ~> s
  where
  go state (Tuple name xs) = name := (popOnConfig xs jsonEmptyObject) ~> state

writePair :: Maybe (Array PairPart) -> Json -> Json
writePair Nothing s = s

writePair (Just xs) s = "PairPotentials" := (popOnPair xs jsonEmptyObject) ~> s

xmlPair :: (Maybe (Array PairPart)) -> XmlNode -> XmlNode
xmlPair Nothing s = s

xmlPair (Just xs) s = xmlActOn "pairPotentials" (map xmlOnPair xs) ::=> s

writeMaster (Just xs) s = "master" := (popOnMaster xs jsonEmptyObject) ~> s

writeMaster Nothing s = s

xmlMaster :: (Maybe (Array MasterPart)) -> XmlNode -> XmlNode
xmlMaster Nothing s = s

xmlMaster (Just xs) s = xmlActOn "master" (map xmlOnMaster xs) ::=> s
