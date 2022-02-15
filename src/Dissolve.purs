module Dissolve where

import Prelude
import Control.Alternative ((<|>))
import Configuration (configuration)
import Data.Either (Either)
import Data.List.NonEmpty (toUnfoldable)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Species (species)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (skipSpaces)
import Types (Section)

section :: Parser String Section
section = species <|> configuration

dissolve :: Parser String (Array Section)
dissolve = skipSpaces *> (toUnfoldable <$> sepBy1 section skipSpaces)

loadDissolveFile :: String -> Aff (Either ParseError (Array Section))
loadDissolveFile file = do
  input <- readTextFile UTF8 file
  pure $ runParser input dissolve
