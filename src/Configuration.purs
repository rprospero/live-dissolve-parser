module Configuration where

import Prelude
import Control.Alternative ((<|>))
import Data.List.NonEmpty (toUnfoldable)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (many1Till)
import Text.Parsing.Parser.String (string)
import Types
import Util (dissolveTokens, signedFloat)

density = dissolveTokens.reserved "Density" *> (GAPDensity <$> signedFloat <*> dissolveTokens.symbol "atoms/A3")

population = dissolveTokens.reserved "Population" *> (GAPPopulation <$> dissolveTokens.integer)

species = dissolveTokens.reserved "Species" *> (GAPSpecies <$> dissolveTokens.stringLiteral)

generatorAddPart = density <|> population <|> species

add = do
  _ <- dissolveTokens.reserved "Add"
  contents <- many1Till generatorAddPart $ string "End"
  _ <- dissolveTokens.reserved "Add"
  pure (GPAdd $ toUnfoldable contents)

generatorPart = add

generator = do
  _ <- dissolveTokens.reserved "Generator"
  contents <- many1Till generatorPart $ string "End"
  _ <- dissolveTokens.reserved "Generator"
  pure (Generator $ toUnfoldable contents)

temperature = dissolveTokens.reserved "Temperature" *> (Temperature <$> dissolveTokens.float)

configurationPart = temperature <|> generator

configuration = do
  _ <- dissolveTokens.reserved "Configuration"
  name <- dissolveTokens.stringLiteral
  contents <- many1Till configurationPart $ string "End"
  _ <- dissolveTokens.reserved "Configuration"
  pure (Configuration name $ toUnfoldable contents)
