module Types where

import Configuration (ConfigurationPart)
import Prelude (class Show)
import Species (SpeciesPart)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Section
  = Species String (Array SpeciesPart)
  | Configuration String (Array ConfigurationPart)

derive instance genericSection :: Generic Section _

instance showSection :: Show Section where
  show x = genericShow x
