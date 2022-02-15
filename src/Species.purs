module Species where

import Prelude
import Control.Alternative ((<|>))
import Data.Array (many)
import Data.List.NonEmpty as List
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (many1Till, optionMaybe)
import Text.Parsing.Parser.String (char, string)
import Util (dissolveTokens, signedFloat, bool)

data SpeciesPart
  = Atom Int String Number Number Number String (Maybe Number)
  | Angle Int Int Int String Number Number
  | Bond Int Int String Number Number
  | Isotopologue String String Int String Int
  | Site String (Array SitePart)

derive instance genericSpeciesPart :: Generic SpeciesPart _

instance showSpeciesPart :: Show SpeciesPart where
  show x = genericShow x

data SitePart
  = Origin (Array Int)
  | XAxis (Array Int)
  | YAxis (Array Int)
  | OriginMassWeighted Boolean

derive instance genericSitePart :: Generic SitePart _

instance showSitePart :: Show SitePart where
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

site :: Parser String SpeciesPart
site = do
  _ <- dissolveTokens.reserved "Site"
  name <- dissolveTokens.stringLiteral
  contents <- many1Till sitePart $ string "End"
  _ <- dissolveTokens.reserved "Site"
  pure (Site name $ List.toUnfoldable contents)

origin = dissolveTokens.symbol "Origin" *> (Origin <$> many dissolveTokens.integer)

xaxis = dissolveTokens.symbol "XAxis" *> (XAxis <$> many dissolveTokens.integer)

yaxis = dissolveTokens.symbol "YAxis" *> (YAxis <$> many dissolveTokens.integer)

massWeighted = dissolveTokens.symbol "OriginMassWeighted" *> (OriginMassWeighted <$> bool)

sitePart = xaxis <|> yaxis <|> massWeighted <|> origin

speciesPart :: Parser String SpeciesPart
speciesPart = atom <|> bond <|> angle <|> isotopologue <|> site
