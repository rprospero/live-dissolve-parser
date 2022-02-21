module Species where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Force
import Foreign.Object
import Prelude
import Control.Alternative ((<|>))
import Data.Array (cons, foldl, many, snoc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser.Combinators (optionMaybe, (<?>))
import Text.Parsing.Parser.String (char)
import Util (MyParser, arbitrary, bool, dissolveTokens, namedContainer, punt, signedFloat, updateArray, updateInner)

data SpeciesPart
  = Atom Int String Number Number Number String (Maybe Number)
  | Angle Int Int Int (Maybe ForceInfo)
  | Bond Int Int (Maybe ForceInfo)
  | Isotopologue (Array String)
  | Torsion Int Int Int Int (Maybe ForceInfo)
  | Improper Int Int Int Int (Maybe ForceInfo)
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

atom :: MyParser SpeciesPart
atom = dissolveTokens.symbol "Atom" *> (Atom <$> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat <*> signedFloat <*> dissolveTokens.stringLiteral <*> optionMaybe signedFloat)

bond :: MyParser SpeciesPart
bond = dissolveTokens.symbol "Bond" *> (Bond <$> dissolveTokens.integer <*> dissolveTokens.integer <*> optionMaybe forceInfo)

angle :: MyParser SpeciesPart
angle = dissolveTokens.symbol "Angle" *> (Angle <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> optionMaybe forceInfo)

torsion :: MyParser SpeciesPart
torsion = dissolveTokens.symbol "Torsion" *> (Torsion <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> optionMaybe forceInfo)

improper = dissolveTokens.symbol "Improper" *> (Improper <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> optionMaybe forceInfo)

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

  go s (Angle i j k ref) = updateArray "angles" (\c -> fromArray $ flip snoc (writeAngle i j k ref) c) s

  go s (Bond i j ref) = updateArray "bonds" (\c -> fromArray $ flip snoc (writeBond i j ref) c) s

  go s (Torsion i j k l ref) = updateArray "torsions" (\c -> fromArray $ flip snoc (writeTorsion i j k l ref) c) s

  go s (Improper i j k l ref) = updateArray "impropers" (\c -> fromArray $ flip snoc (writeTorsion i j k l ref) c) s

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

writeBond i j ref =
  "j"
    := j
    ~> "i"
    := i
    ~> writeRef ref jsonEmptyObject

writeAngle i j k ref =
  "k"
    := k
    ~> "j"
    := j
    ~> "i"
    := i
    ~> writeRef ref jsonEmptyObject

writeTorsion i j k l ref =
  "l" := l
    ~> "k"
    := k
    ~> "j"
    := j
    ~> "i"
    := i
    ~> writeRef ref jsonEmptyObject
