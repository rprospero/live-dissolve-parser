module Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Section
  = Species String (Array SpeciesPart)

data SpeciesPart
  = Atom Int String Number Number Number String Number
  | Angle Int Int Int String Number Number
  | Bond Int Int String Number Number

derive instance genericSpeciesPart :: Generic SpeciesPart _

instance showSpeciesPart :: Show SpeciesPart where
  show x = genericShow x

derive instance genericSection :: Generic Section _

instance showSection :: Show Section where
  show x = genericShow x
