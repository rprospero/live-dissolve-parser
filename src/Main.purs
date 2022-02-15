module Main where

import Prelude
import Control.Alternative ((<|>))
import Data.Array (some)
import Data.CodePoint.Unicode (isAlphaNum)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (toUnfoldable)
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (ParserT, runParser)
import Text.Parsing.Parser.Combinators (many1Till, sepBy, sepBy1)
import Text.Parsing.Parser.String (satisfy, skipSpaces, string)

alphaNums :: forall m. Monad m => ParserT String m String
alphaNums = do
  contents <- some $ satisfy (isAlphaNum <<< codePointFromChar)
  pure $ fromCharArray contents

data Section
  = Section String (Array Section)
  | Line String

derive instance genericSection :: Generic Section _

instance showSection :: Show Section where
  show x = genericShow x

line :: forall m. Monad m => ParserT String m Section
line = Line <$> (string "Foo" <|> string "bar")

realSection :: forall m. Monad m => ParserT String m Section
realSection = do
  name <- alphaNums
  skipSpaces
  contents <- many1Till (section <* skipSpaces) $ string "End"
  _ <- string name
  pure (Section name $ toUnfoldable contents)

dissolve :: forall m. Monad m => ParserT String m (Array Section)
dissolve = toUnfoldable <$> sepBy1 section skipSpaces

section :: forall m. Monad m => ParserT String m Section
section = line <|> realSection

main :: Effect Unit
main =
  let
    result = runParser "Master\nFoo\nHappy\n  bar\nEndHappy\nFoo\nEndMaster" dissolve
  in
    do
      log $ show result
      log "🍝"
