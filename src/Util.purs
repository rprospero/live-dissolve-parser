module Util where

import Prelude
import Control.Alternative ((<|>))
import Data.Array (toUnfoldable, some, many)
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.List as List
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodeUnits as SCU
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, many1Till, (<?>))
import Text.Parsing.Parser.String (char, satisfy, string)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser, alphaNum, letter, makeTokenParser)

type MyParser
  = Parser String

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

myStringChar :: MyParser (Maybe Char)
myStringChar =
  (Just <$> satisfy (\c → (c /= '\'') && (c /= '\n')))
    <|> (string "\\'" *> pure (Just '\''))
    <?> "string character"

myStringLiteral :: (MyParser String -> MyParser String) -> MyParser String
myStringLiteral lex = lex (go <?> "literal string")
  where
  go :: MyParser String
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

signedFloat :: MyParser Number
signedFloat = negativeFloat <|> dissolveTokens.float
  where
  negativeFloat = do
    _ ← char '-'
    x ← dissolveTokens.float
    pure $ -1.0 * x

signedNum :: MyParser (Either Int Number)
signedNum = negativeFloat <|> dissolveTokens.naturalOrFloat
  where
  negativeFloat = do
    _ ← char '-'
    x ← dissolveTokens.naturalOrFloat
    pure $ bimap (-1 * _) (-1.0 * _) x

bool :: MyParser Boolean
bool = t <|> f <?> "Boolean"
  where
  t = dissolveTokens.symbol "True" *> pure true

  f = dissolveTokens.symbol "False" *> pure false

container :: forall a x. String -> MyParser x -> (Array x -> a) -> MyParser a
container name content constructor = do
  _ <- dissolveTokens.reserved name
  contents <- many1Till content $ string "End"
  _ <- dissolveTokens.reserved name
  pure (constructor $ NE.toUnfoldable contents)

namedContainer :: forall a x. String -> MyParser x -> (String -> Array x -> a) -> MyParser a
namedContainer kind content constructor = do
  _ <- dissolveTokens.reserved kind
  name <- dissolveTokens.stringLiteral
  contents <- many1Till content $ string "End"
  _ <- dissolveTokens.reserved kind
  pure (constructor name $ NE.toUnfoldable contents)

sksContainer :: forall a x. String -> MyParser x -> (String -> String -> String -> Array x -> a) -> MyParser a
sksContainer kind content constructor = do
  _ <- dissolveTokens.symbol kind
  file <- dissolveTokens.stringLiteral <?> "file"
  word <- dissolveTokens.identifier
  path <- dissolveTokens.stringLiteral <?> "path"
  contents <- many1Till content $ string "End"
  _ <- dissolveTokens.reserved kind
  pure (constructor file word path $ NE.toUnfoldable contents)

notSpace :: MyParser Char
notSpace = satisfy (\c -> (c /= ' ') && (c /= '\n') && (c /= '\t'))

arbitrary :: MyParser String
arbitrary = do
  text <- fromCharArray <$> some notSpace
  _ <- many (satisfy (\c -> (c == ' ') || (c == '\t')))
  pure text

punt :: forall a. String -> (Array String -> a) -> MyParser a
punt kind constructor = do
  _ <- dissolveTokens.symbol kind
  dissolveTokens.whiteSpace
  contents <- many1Till arbitrary $ char '\n'
  dissolveTokens.whiteSpace
  pure $ constructor $ NE.toUnfoldable contents
