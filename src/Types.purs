module Types where

import Configuration (ConfigurationPart)
import Prelude (class Show)
import Species (SpeciesPart)
import Master (MasterPart)
import PairPotential (PairPart)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Layer (LayerPart)

data Section
  = Species String (Array SpeciesPart)
  | Configuration String (Array ConfigurationPart)
  | PairPotentials (Array PairPart)
  | Layer String (Array LayerPart)
  | Master (Array MasterPart)

derive instance genericSection :: Generic Section _

instance showSection :: Show Section where
  show x = genericShow x
