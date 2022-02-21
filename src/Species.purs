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
import Util
import Xml

data SpeciesPart
  = Atom Int String Number Number Number String (Maybe Number)
  | Angle Int Int Int (Maybe ForceInfo)
  | BondType Int Int String
  | Bond Int Int (Maybe ForceInfo)
  | Isotopologue (Array String)
  | Torsion Int Int Int Int (Maybe ForceInfo)
  | Improper Int Int Int Int (Maybe ForceInfo)
  | Site String (Array SitePart)
  | Forcefield String
  | Noop

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
atom = dissolveTokens.symbol "Atom" *> (Atom <$> dissolveTokens.integer <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat <*> signedFloat <*> allString <*> optionMaybe signedFloat)

bond :: MyParser SpeciesPart
bond = dissolveTokens.symbol "Bond" *> (Bond <$> dissolveTokens.integer <*> dissolveTokens.integer <*> optionMaybe forceInfo)

bondType = dissolveTokens.symbol "BondType" *> (BondType <$> dissolveTokens.integer <*> dissolveTokens.integer <*> allString)

angle :: MyParser SpeciesPart
angle = dissolveTokens.symbol "Angle" *> (Angle <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> optionMaybe forceInfo)

torsion :: MyParser SpeciesPart
torsion = dissolveTokens.symbol "Torsion" *> (Torsion <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> optionMaybe forceInfo)

improper = dissolveTokens.symbol "Improper" *> (Improper <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> optionMaybe forceInfo)

isotopologue :: MyParser SpeciesPart
isotopologue = punt "Isotopologue" Isotopologue

site :: MyParser SpeciesPart
site = namedContainer "Site" sitePart Site

forcefield = dissolveTokens.symbol "Forcefield" *> (Forcefield <$> allString)

noop = (nAtoms <|> nBonds <|> nAngles <|> nTorsions) *> pure Noop
  where
  nAtoms = dissolveTokens.symbol "NAtoms" *> dissolveTokens.integer

  nBonds = dissolveTokens.symbol "NBonds" *> dissolveTokens.integer

  nAngles = dissolveTokens.symbol "NAngles" *> dissolveTokens.integer

  nTorsions = dissolveTokens.symbol "NTorsions" *> dissolveTokens.integer

origin = dissolveTokens.symbol "Origin" *> (Origin <$> many dissolveTokens.integer)

xaxis = dissolveTokens.symbol "XAxis" *> (XAxis <$> many dissolveTokens.integer)

yaxis = dissolveTokens.symbol "YAxis" *> (YAxis <$> many dissolveTokens.integer)

massWeighted = dissolveTokens.symbol "OriginMassWeighted" *> (OriginMassWeighted <$> bool)

sitePart = xaxis <|> yaxis <|> massWeighted <|> origin

speciesPart :: MyParser SpeciesPart
speciesPart = atom <|> bondType <|> bond <|> angle <|> torsion <|> improper <|> isotopologue <|> site <|> forcefield <|> noop <?> "Species Term"

----------------------------------------------------------------------------
popOnSpecies :: Array SpeciesPart -> Json -> Json
popOnSpecies xs s = foldl go s xs
  where
  go s (Atom index element x y z cls charge) = updateArray "atoms" (\c -> fromArray $ flip snoc (writeAtom index element x y z cls charge) c) s

  go s (Angle i j k ref) = updateArray "angles" (\c -> fromArray $ flip snoc (writeAngle i j k ref) c) s

  go s (Bond i j ref) = updateArray "bonds" (\c -> fromArray $ flip snoc (writeBond i j ref) c) s

  go s (BondType i j name) = updateArray "bondTypes" (\c -> fromArray $ flip snoc (writeBondType i j name) c) s

  go s (Torsion i j k l ref) = updateArray "torsions" (\c -> fromArray $ flip snoc (writeTorsion i j k l ref) c) s

  go s (Improper i j k l ref) = updateArray "impropers" (\c -> fromArray $ flip snoc (writeTorsion i j k l ref) c) s

  go s (Isotopologue as) = updateArray "isotopologues" (\c -> fromArray $ cons (encodeJson as) c) s

  go s (Site name vs) = updateInner "site" (writeSite name vs) s

  go s (Forcefield name) = "forcefield" := name ~> s

  go s (Noop) = s

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

writeBondType i j name =
  "type" := name
    ~> "j"
    := j
    ~> "i"
    := i
    ~> jsonEmptyObject

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

xmlOnSpecies :: SpeciesPart -> XmlNode -> XmlNode
xmlOnSpecies (Forcefield name) s = ("forcefield" ::= name) s

xmlOnSpecies (Bond i j ref) s = (xmlRef ref <<< ("i" ::= i) <<< ("j" ::= j) $ xmlEmptyNode "bond") ::=> s

xmlOnSpecies (Angle i j k ref) s = (xmlRef ref <<< ("i" ::= i) <<< ("j" ::= j) <<< ("k" ::= k) $ xmlEmptyNode "angle") ::=> s

xmlOnSpecies (Torsion i j k l ref) s = (xmlRef ref <<< ("i" ::= i) <<< ("j" ::= j) <<< ("k" ::= k) <<< ("l" ::= l) $ xmlEmptyNode "torsion") ::=> s

xmlOnSpecies (Improper i j k l ref) s = (xmlRef ref <<< ("i" ::= i) <<< ("j" ::= j) <<< ("k" ::= k) <<< ("l" ::= l) $ xmlEmptyNode "angle") ::=> s

-- go s (Atom index element x y z cls charge) = updateArray "atoms" (\c -> fromArray $ flip snoc (writeAtom index element x y z cls charge) c) s
-- go s (BondType i j name) = updateArray "bondTypes" (\c -> fromArray $ flip snoc (writeBondType i j name) c) s
-- go s (Isotopologue as) = updateArray "isotopologues" (\c -> fromArray $ cons (encodeJson as) c) s
-- go s (Site name vs) = updateInner "site" (writeSite name vs) s
xmlOnSpecies _ s = s
