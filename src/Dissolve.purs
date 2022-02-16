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
import Text.Parsing.Parser (ParseError, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (skipSpaces)
import Types (Section(..))
import Util (container, dissolveTokens, namedContainer, MyParser)

section :: MyParser Section
section = species <|> configuration <|> pairPotentials <|> layer <|> master

dissolve :: MyParser (Array Section)
dissolve = dissolveTokens.whiteSpace *> (toUnfoldable <$> sepBy1 section skipSpaces)

loadDissolveFile :: String -> Aff (Either ParseError (Array Section))
loadDissolveFile file = do
  input <- readTextFile UTF8 file
  pure $ runParser input dissolve

configuration :: MyParser Section
configuration = namedContainer "Configuration" configurationPart Configuration

species :: MyParser Section
species = namedContainer "Species" speciesPart Species

pairPotentials :: MyParser Section
pairPotentials = container "PairPotentials" pairPart PairPotentials

layer :: MyParser Section
layer = namedContainer "Layer" layerPart Layer

master :: MyParser Section
master = container "Master" masterPart Master
