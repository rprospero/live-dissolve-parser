module Configuration where

import Prelude
import Control.Alternative ((<|>))
import Data.Array (many)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser.String (noneOf, skipSpaces, string)
import Util (container, dissolveTokens, signedFloat)

data ConfigurationPart
  = Generator (Array GeneratorPart)
  | Temperature Number
  | InputCoordinates String String

derive instance genericConfigurationPart :: Generic ConfigurationPart _

instance showConfigurationPart :: Show ConfigurationPart where
  show x = genericShow x

data GeneratorPart
  = Add (Array GeneratorAddPart)
  | Box (Array BoxPart)

derive instance genericGeneratorPart :: Generic GeneratorPart _

instance showGeneratorPart :: Show GeneratorPart where
  show x = genericShow x

data GeneratorAddPart
  = Density Number String
  | Population Int
  | Species String
  | BoxAction String

derive instance genericGeneratorAddPart :: Generic GeneratorAddPart _

instance showGeneratorAddPart :: Show GeneratorAddPart where
  show x = genericShow x

data BoxPart
  = Length Number Number Number

derive instance genericBoxPart :: Generic BoxPart _

instance showBoxPart :: Show BoxPart where
  show x = genericShow x

density = dissolveTokens.symbol "Density" *> (Density <$> signedFloat <*> dissolveTokens.symbol "atoms/A3")

population = dissolveTokens.symbol "Population" *> (Population <$> dissolveTokens.integer)

species = dissolveTokens.symbol "Species" *> (Species <$> dissolveTokens.stringLiteral)

boxAction = dissolveTokens.symbol "BoxAction" *> (BoxAction <$> dissolveTokens.identifier)

generatorAddPart = density <|> population <|> species <|> boxAction

add = container "Add" generatorAddPart Add

length = dissolveTokens.symbol "Lengths" *> (Length <$> signedFloat <*> signedFloat <*> signedFloat)

boxPart = length

box = container "Box" boxPart Box

generatorPart = add <|> box

generator = container "Generator" generatorPart Generator

temperature = dissolveTokens.symbol "Temperature" *> (Temperature <$> dissolveTokens.float)

inputCoordinates = do
  _ <- dissolveTokens.symbol "InputCoordinates"
  name <- dissolveTokens.identifier
  path <- many $ noneOf [ ' ', '\n', '\t' ]
  skipSpaces
  _ <- string "End"
  _ <- dissolveTokens.symbol "InputCoordinates"
  pure (InputCoordinates name $ fromCharArray path)

configurationPart = temperature <|> generator <|> inputCoordinates
