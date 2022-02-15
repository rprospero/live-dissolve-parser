module Species where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Text.Parsing.Parser.String (char)
import Util (dissolveTokens, signedFloat)

data SpeciesPart
  = Atom Int String Number Number Number String (Maybe Number)
  | Angle Int Int Int String Number Number
  | Bond Int Int String Number Number
  | Isotopologue String String Int String Int

derive instance genericSpeciesPart :: Generic SpeciesPart _

instance showSpeciesPart :: Show SpeciesPart where
  show x = genericShow x

atom :: Parser String SpeciesPart
atom = dissolveTokens.symbol "Atom" *> (Atom <$> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat <*> signedFloat <*> dissolveTokens.stringLiteral <*> optionMaybe signedFloat)

bond :: Parser String SpeciesPart
bond = dissolveTokens.symbol "Bond" *> (Bond <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat)

angle :: Parser String SpeciesPart
angle = dissolveTokens.symbol "Angle" *> (Angle <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat)

isotopologue :: Parser String SpeciesPart
isotopologue = do
  _ <- dissolveTokens.symbol "Isotopologue"
  name <- dissolveTokens.stringLiteral
  alpha <- dissolveTokens.identifier
  _ <- char '='
  nAlpha <- dissolveTokens.integer
  beta <- dissolveTokens.identifier
  _ <- char '='
  nBeta <- dissolveTokens.integer
  pure $ Isotopologue name alpha nAlpha beta nBeta

speciesPart :: Parser String SpeciesPart
speciesPart = atom <|> bond <|> angle <|> isotopologue
