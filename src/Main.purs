module Main where

import Prelude
import Species (species)
import Data.List.NonEmpty (toUnfoldable)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (skipSpaces)
import Types (Section)

section :: Parser String Section
section = species

dissolve :: Parser String (Array Section)
dissolve = skipSpaces *> (toUnfoldable <$> sepBy1 section skipSpaces)

main :: Effect Unit
main = do
  input ← readTextFile UTF8 "examples/input.txt"
  log $ show $ runParser input dissolve
  log "🍝"
