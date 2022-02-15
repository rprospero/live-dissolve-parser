module Dissolve where

import Prelude
import Configuration (configurationPart)
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
section = species <|> configuration

dissolve :: Parser String (Array Section)
dissolve = skipSpaces *> (toUnfoldable <$> sepBy1 section skipSpaces)

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
