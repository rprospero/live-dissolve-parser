module Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Section
  = Species String (Array SpeciesPart)
  | Configuration String (Array ConfigurationPart)

derive instance genericSection :: Generic Section _

instance showSection :: Show Section where
  show x = genericShow x

data SpeciesPart
  = Atom Int String Number Number Number String Number
  | Angle Int Int Int String Number Number
  | Bond Int Int String Number Number

derive instance genericSpeciesPart :: Generic SpeciesPart _

instance showSpeciesPart :: Show SpeciesPart where
  show x = genericShow x

data ConfigurationPart
  = Generator (Array GeneratorPart)
  | Temperature Number

derive instance genericConfigurationPart :: Generic ConfigurationPart _

instance showConfigurationPart :: Show ConfigurationPart where
  show x = genericShow x

data GeneratorPart
  = GPAdd (Array GeneratorAddPart)

derive instance genericGeneratorPart :: Generic GeneratorPart _

instance showGeneratorPart :: Show GeneratorPart where
  show x = genericShow x

data GeneratorAddPart
  = GAPDensity Number String
  | GAPPopulation Int
  | GAPSpecies String

derive instance genericGeneratorAddPart :: Generic GeneratorAddPart _

instance showGeneratorAddPart :: Show GeneratorAddPart where
  show x = genericShow x
