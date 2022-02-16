module Util where

import Prelude
import Control.Alternative ((<|>))
import Data.Array (toUnfoldable)
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, many1Till, (<?>))
import Text.Parsing.Parser.String (char, satisfy, string)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser, alphaNum, letter, makeTokenParser)

dissolveLanguage ∷ LanguageDef
dissolveLanguage =
  LanguageDef
    { caseSensitive: false
    , commentEnd: "*/"
    , commentLine: "#"
    , commentStart: "/*"
    , identLetter: alphaNum
    , identStart: letter
    , nestedComments: false
    , opLetter: (fail "No Operators")
    , opStart: (fail "NoOperators")
    , reservedNames: []
    , reservedOpNames: []
    }

myStringChar :: Parser String (Maybe Char)
myStringChar =
  (Just <$> satisfy (\c → (c /= '\'') && (c /= '\n')))
    <|> (string "\\'" *> pure (Just '\''))
    <?> "string character"

myStringLiteral :: (Parser String String -> Parser String String) -> Parser String String
myStringLiteral lex = lex (go <?> "literal string")
  where
  go :: Parser String String
  go = do
    maybeChars <- between (char '\'') (char '\'' <?> "end of string") (List.many myStringChar)
    pure $ SCU.fromCharArray $ List.toUnfoldable $ List.foldr folder List.Nil maybeChars

  folder :: Maybe Char -> List.List Char -> List.List Char
  folder Nothing chars = chars

  folder (Just c) chars = List.Cons c chars

dissolveTokens :: TokenParser
dissolveTokens =
  let
    base = (makeTokenParser dissolveLanguage)
  in
    base { stringLiteral = myStringLiteral base.lexeme }

signedFloat :: Parser String Number
signedFloat = negativeFloat <|> dissolveTokens.float
  where
  negativeFloat = do
    _ ← char '-'
    x ← dissolveTokens.float
    pure $ -1.0 * x

bool :: Parser String Boolean
bool = t <|> f <?> "Boolean"
  where
  t = dissolveTokens.symbol "True" *> pure true

  f = dissolveTokens.symbol "False" *> pure false

container :: forall a x. String -> Parser String x -> (Array x -> a) -> Parser String a
container name content constructor = do
  _ <- dissolveTokens.reserved name
  contents <- many1Till content $ string "End"
  _ <- dissolveTokens.reserved name
  pure (constructor $ NE.toUnfoldable contents)

namedContainer :: forall a x. String -> Parser String x -> (String -> Array x -> a) -> Parser String a
namedContainer kind content constructor = do
  _ <- dissolveTokens.reserved kind
  name <- dissolveTokens.stringLiteral
  contents <- many1Till content $ string "End"
  _ <- dissolveTokens.reserved kind
  pure (constructor name $ NE.toUnfoldable contents)
