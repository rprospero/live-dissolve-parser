module Species where

import Prelude
import Util (dissolveTokens, signedFloat)
import Control.Alternative ((<|>))
import Data.List.NonEmpty (toUnfoldable)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (many1Till)
import Text.Parsing.Parser.String (string)
import Types (Section(..), SpeciesPart(..))

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
