module Types where

import Configuration
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Lens
import Foreign.Object
import Layer
import Master
import PairPotential
import Prelude
import Species
import Xml
import Control.Monad.State (State, execState, modify)
import Data.Array (foldl)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
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
          xmlSpecies species
          _ <- modify (xmlLayer layer)
          _ <- modify (xmlConfig config)
          xmlPair pair
          xmlMaster master

writeLayer :: (Array (Tuple String (Array LayerPart))) -> Json -> Json
writeLayer xs s = "layer" := foldl go jsonEmptyObject xs ~> s
  where
  go state (Tuple name xs) = name := (popOnLayer xs jsonEmptyObject) ~> state

xmlLayer :: (Array (Tuple String (Array LayerPart))) -> XmlNode -> XmlNode
xmlLayer xs s = foldl go s xs
  where
  go state (Tuple name ss) = (foldl (flip xmlOnLayer) ("name" ::= name $ xmlEmptyNode "layer") ss) ::=> state

writeSpecies :: (Array (Tuple String (Array SpeciesPart))) -> Json -> Json
writeSpecies xs s = "species" := foldl go jsonEmptyObject xs ~> s
  where
  go state (Tuple name xs) = name := (popOnSpecies xs jsonEmptyObject) ~> state

xmlSpecies :: (Array (Tuple String (Array SpeciesPart))) -> State XmlNode Unit
xmlSpecies xs = for_ xs go
  where
  go (Tuple name ss) = onNewChild "species" $ onAttr "name" name *> for_ ss xmlOnSpecies

writeConfig :: (Array (Tuple String (Array ConfigurationPart))) -> Json -> Json
writeConfig xs s = "configuration" := foldl go jsonEmptyObject xs ~> s
  where
  go state (Tuple name xs) = name := (popOnConfig xs jsonEmptyObject) ~> state

xmlConfig :: (Array (Tuple String (Array ConfigurationPart))) -> XmlNode -> XmlNode
xmlConfig xs s = foldl go s xs
  where
  go state (Tuple name ss) = (foldl (flip xmlOnConfig) ("name" ::= name $ xmlEmptyNode "configuration") ss) ::=> state

writePair :: Maybe (Array PairPart) -> Json -> Json
writePair Nothing s = s

writePair (Just xs) s = "PairPotentials" := (popOnPair xs jsonEmptyObject) ~> s

xmlPair Nothing = pure unit

xmlPair (Just xs) = onNewChild "pairPotentials" $ for_ xs xmlOnPair

writeMaster (Just xs) s = "master" := (popOnMaster xs jsonEmptyObject) ~> s

writeMaster Nothing s = s

xmlMaster :: (Maybe (Array MasterPart)) -> State XmlNode Unit
xmlMaster Nothing = pure unit

xmlMaster (Just xs) = onNewChild "master" $ for_ xs xmlOnMaster
