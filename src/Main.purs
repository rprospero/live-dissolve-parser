module Main where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.List as List
import Data.List.NonEmpty (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (between, many1Till, sepBy1, (<?>))
import Text.Parsing.Parser.String (char, satisfy, skipSpaces, string)
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

data Section
  = Species String (Array SpeciesPart)

data SpeciesPart
  = Atom Int String Number Number Number String Number
  | Angle Int Int Int String Number Number
  | Bond Int Int String Number Number

derive instance genericSpeciesPart :: Generic SpeciesPart _

instance showSpeciesPart :: Show SpeciesPart where
  show x = genericShow x

derive instance genericSection :: Generic Section _

instance showSection :: Show Section where
  show x = genericShow x

signedFloat :: Parser String Number
signedFloat = negativeFloat <|> dissolveTokens.float
  where
  negativeFloat = do
    _ ← char '-'
    x ← dissolveTokens.float
    pure $ -1.0 * x

speciesPartAtom :: Parser String SpeciesPart
speciesPartAtom = dissolveTokens.reserved "Atom" *> (Atom <$> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat <*> signedFloat <*> dissolveTokens.stringLiteral <*> signedFloat)

speciesPartBond :: Parser String SpeciesPart
speciesPartBond = dissolveTokens.reserved "Bond" *> (Bond <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat)

speciesPartAngle :: Parser String SpeciesPart
speciesPartAngle = dissolveTokens.reserved "Angle" *> (Angle <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat)

speciesPart :: Parser String SpeciesPart
speciesPart = speciesPartAtom <|> speciesPartBond <|> speciesPartAngle

species :: Parser String Section
species = do
  _ <- dissolveTokens.reserved "Species"
  name ← dissolveTokens.stringLiteral
  contents <- many1Till speciesPart $ string "End"
  _ <- dissolveTokens.reserved "Species"
  pure (Species name $ toUnfoldable contents)

section :: Parser String Section
section = species

dissolve :: Parser String (Array Section)
dissolve = skipSpaces *> (toUnfoldable <$> sepBy1 section skipSpaces)

main :: Effect Unit
main = do
  input ← readTextFile UTF8 "examples/input.txt"
  log $ show $ runParser input dissolve
  log "🍝"
