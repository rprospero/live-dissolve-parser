module Dissolve where

import Prelude
import Configuration (configurationPart)
import Control.Alternative ((<|>))
import Data.Either (Either)
import Data.List.NonEmpty (toUnfoldable)
import Effect.Aff (Aff)
import Layer (layerPart)
import Master (masterPart)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import PairPotential (pairPart)
import Species (speciesPart)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (skipSpaces)
import Types (Section(..))
import Util (container, dissolveTokens, namedContainer)

section :: Parser String Section
section = species <|> configuration <|> pairPotentials <|> layer <|> master

dissolve :: Parser String (Array Section)
dissolve = dissolveTokens.whiteSpace *> (toUnfoldable <$> sepBy1 section skipSpaces)

loadDissolveFile :: String -> Aff (Either ParseError (Array Section))
loadDissolveFile file = do
  input <- readTextFile UTF8 file
  pure $ runParser input dissolve

configuration :: Parser String Section
configuration = namedContainer "Configuration" configurationPart Configuration

species :: Parser String Section
species = namedContainer "Species" speciesPart Species

pairPotentials :: Parser String Section
pairPotentials = container "PairPotentials" pairPart PairPotentials

layer :: Parser String Section
layer = namedContainer "Layer" layerPart Layer

master :: Parser String Section
master = container "Master" masterPart Master
