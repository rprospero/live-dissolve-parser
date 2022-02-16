module Species where

import Prelude
import Control.Alternative ((<|>))
import Data.Array (many)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Text.Parsing.Parser.String (char)
import Util (bool, dissolveTokens, namedContainer, signedFloat, MyParser)

data SpeciesPart
  = Atom Int String Number Number Number String (Maybe Number)
  | Angle Int Int Int AngleInfo
  | Bond Int Int BondInfo
  | Isotopologue String String Int String Int
  | Site String (Array SitePart)

derive instance genericSpeciesPart :: Generic SpeciesPart _

instance showSpeciesPart :: Show SpeciesPart where
  show x = genericShow x

data BondInfo
  = BondRef String
  | BondInfo String Number Number

derive instance genericBondInfo :: Generic BondInfo _

instance showBondInfo :: Show BondInfo where
  show x = genericShow x

data AngleInfo
  = AngleRef String
  | AngleInfo String Number Number

derive instance genericAngleInfo :: Generic AngleInfo _

instance showAngleInfo :: Show AngleInfo where
  show x = genericShow x

data SitePart
  = Origin (Array Int)
  | XAxis (Array Int)
  | YAxis (Array Int)
  | OriginMassWeighted Boolean

derive instance genericSitePart :: Generic SitePart _

instance showSitePart :: Show SitePart where
  show x = genericShow x

atom :: MyParser SpeciesPart
atom = dissolveTokens.symbol "Atom" *> (Atom <$> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat <*> signedFloat <*> dissolveTokens.stringLiteral <*> optionMaybe signedFloat)

bond :: MyParser SpeciesPart
bond = dissolveTokens.symbol "Bond" *> (Bond <$> dissolveTokens.integer <*> dissolveTokens.integer <*> bondRef)

bondRef :: MyParser BondInfo
bondRef = master <|> raw
  where
  master = char '@' *> (BondRef <$> dissolveTokens.identifier)

  raw = BondInfo <$> dissolveTokens.identifier <*> signedFloat <*> signedFloat

angle :: MyParser SpeciesPart
angle = dissolveTokens.symbol "Angle" *> (Angle <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> angleRef)

angleRef :: MyParser AngleInfo
angleRef = master <|> raw
  where
  master = char '@' *> (AngleRef <$> dissolveTokens.identifier)

  raw = AngleInfo <$> dissolveTokens.identifier <*> signedFloat <*> signedFloat

isotopologue :: MyParser SpeciesPart
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

site :: MyParser SpeciesPart
site = namedContainer "Site" sitePart Site

origin = dissolveTokens.symbol "Origin" *> (Origin <$> many dissolveTokens.integer)

xaxis = dissolveTokens.symbol "XAxis" *> (XAxis <$> many dissolveTokens.integer)

yaxis = dissolveTokens.symbol "YAxis" *> (YAxis <$> many dissolveTokens.integer)

massWeighted = dissolveTokens.symbol "OriginMassWeighted" *> (OriginMassWeighted <$> bool)

sitePart = xaxis <|> yaxis <|> massWeighted <|> origin

speciesPart :: MyParser SpeciesPart
speciesPart = atom <|> bond <|> angle <|> isotopologue <|> site
