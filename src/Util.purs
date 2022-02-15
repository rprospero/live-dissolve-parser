module Util where

import Prelude
import Control.Alternative ((<|>))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, (<?>))
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
    , reservedNames: [ "Atom", "Bond", "Species" ]
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
