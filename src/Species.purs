module Species where

import Prelude
import Util (dissolveTokens, signedFloat)
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser (Parser)

data SpeciesPart
  = Atom Int String Number Number Number String Number
  | Angle Int Int Int String Number Number
  | Bond Int Int String Number Number

derive instance genericSpeciesPart :: Generic SpeciesPart _

instance showSpeciesPart :: Show SpeciesPart where
  show x = genericShow x

speciesPartAtom :: Parser String SpeciesPart
speciesPartAtom = dissolveTokens.symbol "Atom" *> (Atom <$> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat <*> signedFloat <*> dissolveTokens.stringLiteral <*> signedFloat)

speciesPartBond :: Parser String SpeciesPart
speciesPartBond = dissolveTokens.symbol "Bond" *> (Bond <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat)

speciesPartAngle :: Parser String SpeciesPart
speciesPartAngle = dissolveTokens.symbol "Angle" *> (Angle <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat)

speciesPart :: Parser String SpeciesPart
speciesPart = speciesPartAtom <|> speciesPartBond <|> speciesPartAngle
