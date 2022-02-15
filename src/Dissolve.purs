module Dissolve where

import Prelude
import Layer (layerPart)
import Configuration (configurationPart)
import PairPotential (pairPart)
import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.List.NonEmpty (toUnfoldable)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Species (speciesPart)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy1, many1Till)
import Text.Parsing.Parser.String (skipSpaces, string)
import Types (Section(..))
import Util (dissolveTokens)

section :: Parser String Section
section = species <|> configuration <|> pairPotentials <|> layer

dissolve :: Parser String (Array Section)
dissolve = dissolveTokens.whiteSpace *> (toUnfoldable <$> sepBy1 section skipSpaces)

loadDissolveFile :: String -> Aff (Either ParseError (Array Section))
loadDissolveFile file = do
  input <- readTextFile UTF8 file
  pure $ runParser input dissolve

configuration :: Parser String Section
configuration = do
  _ <- dissolveTokens.reserved "Configuration"
  name <- dissolveTokens.stringLiteral
  contents <- many1Till configurationPart $ string "End"
  _ <- dissolveTokens.reserved "Configuration"
  pure (Configuration name $ toUnfoldable contents)

species :: Parser String Section
species = do
  _ <- dissolveTokens.reserved "Species"
  name ← dissolveTokens.stringLiteral
  contents <- many1Till speciesPart $ string "End"
  _ <- dissolveTokens.reserved "Species"
  pure (Species name $ toUnfoldable contents)

pairPotentials :: Parser String Section
pairPotentials = do
  _ <- dissolveTokens.reserved "PairPotentials"
  contents <- many1Till pairPart $ string "End"
  _ <- dissolveTokens.reserved "PairPotentials"
  pure (PairPotentials $ toUnfoldable contents)

layer :: Parser String Section
layer = do
  _ <- dissolveTokens.reserved "Layer"
  name ← dissolveTokens.stringLiteral
  contents <- many1Till layerPart $ string "End"
  _ <- dissolveTokens.reserved "Layer"
  pure (Layer name $ toUnfoldable contents)
