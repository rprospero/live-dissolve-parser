module Species where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Foreign.Object
import Prelude
import Control.Alternative ((<|>))
import Data.Array (cons, foldl, many, snoc)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (many1Till, optionMaybe, try, (<?>))
import Text.Parsing.Parser.String (char, satisfy, string)
import Text.Parsing.Parser.Token (letter)
import Util (MyParser, arbitrary, bool, dissolveTokens, namedContainer, notSpace, punt, signedFloat, updateArray, updateInner)

data SpeciesPart
  = Atom Int String Number Number Number String (Maybe Number)
  | Angle (Array String)
  | Bond (Array String)
  | Isotopologue (Array String)
  | Torsion (Array String)
  | Improper (Array String)
  | Site String (Array SitePart)

derive instance genericSpeciesPart :: Generic SpeciesPart _

instance showSpeciesPart :: Show SpeciesPart where
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
bond = punt "Bond" Bond

angle :: MyParser SpeciesPart
angle = punt "Angle" Angle

angleRef :: MyParser AngleInfo
angleRef = master <|> raw
  where
  master = char '@' *> (AngleRef <$> arbitrary <* dissolveTokens.whiteSpace)

  raw = AngleInfo <$> dissolveTokens.identifier <*> signedFloat <*> signedFloat

torsion :: MyParser SpeciesPart
torsion = punt "Torsion" Torsion

improper = punt "Improper" Improper

isotopologue :: MyParser SpeciesPart
isotopologue = punt "Isotopologue" Isotopologue

site :: MyParser SpeciesPart
site = namedContainer "Site" sitePart Site

origin = dissolveTokens.symbol "Origin" *> (Origin <$> many dissolveTokens.integer)

xaxis = dissolveTokens.symbol "XAxis" *> (XAxis <$> many dissolveTokens.integer)

yaxis = dissolveTokens.symbol "YAxis" *> (YAxis <$> many dissolveTokens.integer)

massWeighted = dissolveTokens.symbol "OriginMassWeighted" *> (OriginMassWeighted <$> bool)

sitePart = xaxis <|> yaxis <|> massWeighted <|> origin

speciesPart :: MyParser SpeciesPart
speciesPart = atom <|> bond <|> angle <|> torsion <|> improper <|> isotopologue <|> site <?> "Species Term"

----------------------------------------------------------------------------
popOnSpecies :: Array SpeciesPart -> Json -> Json
popOnSpecies xs s = foldl go s xs
  where
  go s (Atom index element x y z cls charge) = updateArray "atoms" (\c -> fromArray $ flip snoc (writeAtom index element x y z cls charge) c) s

  go s (Angle as) = updateArray "angles" (\c -> fromArray $ cons (encodeJson as) c) s

  go s (Bond as) = updateArray "bonds" (\c -> fromArray $ cons (encodeJson as) c) s

  go s (Torsion as) = updateArray "torsions" (\c -> fromArray $ cons (encodeJson as) c) s

  go s (Improper as) = updateArray "impropers" (\c -> fromArray $ cons (encodeJson as) c) s

  go s (Isotopologue as) = updateArray "isotopologues" (\c -> fromArray $ cons (encodeJson as) c) s

  go s (Site name vs) = updateInner "site" (writeSite name vs) s

writeSite name vs s = name := (foldl go jsonEmptyObject vs) ~> s
  where
  go s (Origin os) = "origin" := os ~> s

  go s (XAxis os) = "xAxis" := os ~> s

  go s (YAxis os) = "yAxis" := os ~> s

  go s (OriginMassWeighted os) = "originMassWeighted" := os ~> s

writeAtom index element x y z cls charge =
  "index"
    := index
    ~> "element"
    := element
    ~> "position"
    := [ x, y, z ]
    ~> "label"
    := cls
    ~> "charge"
    :=? charge
    ~>? jsonEmptyObject
