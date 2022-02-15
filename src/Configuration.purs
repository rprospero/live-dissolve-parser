module Configuration where

import Prelude
import Control.Alternative ((<|>))
import Data.List.NonEmpty (toUnfoldable)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (many1Till)
import Text.Parsing.Parser.String (string)
import Util (dissolveTokens, signedFloat)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data ConfigurationPart
  = Generator (Array GeneratorPart)
  | Temperature Number

derive instance genericConfigurationPart :: Generic ConfigurationPart _

instance showConfigurationPart :: Show ConfigurationPart where
  show x = genericShow x

data GeneratorPart
  = Add (Array GeneratorAddPart)

derive instance genericGeneratorPart :: Generic GeneratorPart _

instance showGeneratorPart :: Show GeneratorPart where
  show x = genericShow x

data GeneratorAddPart
  = Density Number String
  | Population Int
  | Species String

derive instance genericGeneratorAddPart :: Generic GeneratorAddPart _

instance showGeneratorAddPart :: Show GeneratorAddPart where
  show x = genericShow x

density = dissolveTokens.symbol "Density" *> (Density <$> signedFloat <*> dissolveTokens.symbol "atoms/A3")

population = dissolveTokens.symbol "Population" *> (Population <$> dissolveTokens.integer)

species = dissolveTokens.symbol "Species" *> (Species <$> dissolveTokens.stringLiteral)

generatorAddPart = density <|> population <|> species

add = do
  _ <- dissolveTokens.symbol "Add"
  contents <- many1Till generatorAddPart $ string "End"
  _ <- dissolveTokens.symbol "Add"
  pure (Add $ toUnfoldable contents)

generatorPart = add

generator = do
  _ <- dissolveTokens.symbol "Generator"
  contents <- many1Till generatorPart $ string "End"
  _ <- dissolveTokens.symbol "Generator"
  pure (Generator $ toUnfoldable contents)

temperature = dissolveTokens.symbol "Temperature" *> (Temperature <$> dissolveTokens.float)

configurationPart = temperature <|> generator
