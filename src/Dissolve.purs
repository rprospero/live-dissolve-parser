module Dissolve where

import Prelude
import Species (species)
import Types (Section)
import Data.Either (Either)
import Data.List.NonEmpty (toUnfoldable)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (skipSpaces)

section :: Parser String Section
section = species

dissolve :: Parser String (Array Section)
dissolve = skipSpaces *> (toUnfoldable <$> sepBy1 section skipSpaces)

loadDissolveFile :: String -> Aff (Either ParseError (Array Section))
loadDissolveFile file = do
  input <- readTextFile UTF8 file
  pure $ runParser input dissolve
