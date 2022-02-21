module Layer where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Foreign.Object
import Prelude
import Util
import Analyser (AnalyserPart, analyserPart)
import Control.Alternative ((<|>))
import Data.Array (foldl, many, snoc)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser.Combinators (between, many1Till, optional, optionMaybe, skipMany, try, (<?>))
import Text.Parsing.Parser.String (char, satisfy, string, whiteSpace)

data LayerPart
  = Module (Array String) (Array ModulePart)
  | LayerFrequency Int

derive instance genericLayerPart :: Generic LayerPart _

instance showLayerPart :: Show LayerPart where
  show x = genericShow x

data ModulePart
  = Configuration String
  | Frequency Int
  | Distance Int Int Number
  | Format String String
  | BinWidth Number
  | IntraBroadening String
  | Averaging Int
  | AveragingScheme String
  | Target String
  | Data String
  | SiteA String String (Maybe (Tuple String String))
  | SiteB String String (Maybe (Tuple String String))
  | Site (Array String)
  | DistanceRange Number Number Number
  | AngleRange Number Number Number
  | ExcludeSameMolecule Boolean
  | InternalData1D String String
  | RangeX Number Number Number
  | RangeY Number Number Number
  | RangeZ Number Number Number
  | Range Number
  | Multiplicity Int Int Int
  | QBroadening (Array String)
  | QDelta Number
  | QMax Number
  | QMin Number
  | TestReflections String
  | Method String
  | SourceRDF String
  | SourceRDFs String
  | WindowFunction String
  | IncludeBragg String
  | BraggQBroadening String Number Number
  | SourceSQs String
  | Data1D String String String (Array Data1DPart)
  | Threshold Number
  | Isotopologue (Array String)
  | SampledDouble String Number
  | SampledVector String String String
  | ErrorType String
  | RangeA Number Number
  | RangeB Number Number
  | RangeBEnabled Boolean
  | Export String String (Array Data1DPart)
  | Save String
  | OnlyWhenEnergyStable Boolean
  | EReq Number
  | InpAFile String
  | NPItSs Int
  | Feedback Number
  | PCofFile String
  | ReferenceFTQMin Number
  | ReferenceFTQMax Number
  | ReferenceNormalisation String
  | ReferenceWindowFunction String
  | SaveRepresentativeGR Boolean
  | SaveEstimatedPartials Boolean
  | SaveReference Boolean
  | SaveSQ Boolean
  | OverwritePotentials Boolean
  | Normalisation String
  | ExpansionFunction String
  | Test Boolean
  | TestAbsEnergyEP String Number
  | TestAnalytic Boolean
  | TestReference String String (Array Data1DPart)
  | TestReferenceInter (Array String)
  | TestReferenceIntra Number
  | TestThreshold Number
  | Species String
  | AtomType Int String
  | TotalCharge Number
  | Bond Int Int Number Number
  | Angle Int Int Int (Array Number)
  | Torsion Int Int Int Int (Array Number)
  | Improper Int Int Int Int (Array Number)
  | NSteps Int
  | InternalTest Boolean
  | Exchangeable (Array String)
  | Reference String String (Array Data1DPart)
  | Analyser (Array AnalyserPart)
  | RawNum Number

derive instance genericModulePart :: Generic ModulePart _

instance showModulePart :: Show ModulePart where
  show x = genericShow x

data Data1DPart
  = Y Int
  | XMin Number

derive instance genericData1Part :: Generic Data1DPart _

instance showData1DPart :: Show Data1DPart where
  show x = genericShow x

configuration :: MyParser ModulePart
configuration = (dissolveTokens.symbol "Configurations" <|> dissolveTokens.symbol "Configuration") *> (Configuration <$> dissolveTokens.stringLiteral)

frequency :: MyParser ModulePart
frequency = dissolveTokens.symbol "Frequency" *> (Frequency <$> dissolveTokens.integer)

distance :: MyParser ModulePart
distance = dissolveTokens.symbol "Distance" *> (Distance <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.float)

format :: MyParser ModulePart
format = do
  _ <- dissolveTokens.reserved "Format"
  kind <- dissolveTokens.identifier
  file <- dissolveTokens.stringLiteral
  -- contents <- many1Till modulePart $ string "End"
  _ <- dissolveTokens.reserved "EndFormat"
  pure (Format kind file)

binWidth :: MyParser ModulePart
binWidth = dissolveTokens.symbol "BinWidth" *> (BinWidth <$> dissolveTokens.float)

intraBroadening :: MyParser ModulePart
intraBroadening = dissolveTokens.symbol "IntraBroadening" *> (IntraBroadening <$> allString)

averaging :: MyParser ModulePart
averaging = dissolveTokens.symbol "Averaging" *> (Averaging <$> dissolveTokens.integer)

averagingScheme = dissolveTokens.symbol "AveragingScheme" *> (AveragingScheme <$> dissolveTokens.identifier)

target :: MyParser ModulePart
target = dissolveTokens.symbol "Target" *> (Target <$> dissolveTokens.stringLiteral)

data_ :: MyParser ModulePart
data_ = dissolveTokens.symbol "Data" *> (Data <$> dissolveTokens.stringLiteral)

siteA :: MyParser ModulePart
siteA = do
  _ <- dissolveTokens.symbol "SiteA"
  x <- dissolveTokens.identifier
  y <- dissolveTokens.stringLiteral
  second <- optionMaybe (Tuple <$> dissolveTokens.symbol x <*> dissolveTokens.stringLiteral)
  pure $ SiteA x y second

siteB :: MyParser ModulePart
siteB = do
  _ <- dissolveTokens.symbol "SiteB"
  x <- dissolveTokens.identifier
  y <- dissolveTokens.stringLiteral
  second <- optionMaybe (Tuple <$> dissolveTokens.symbol x <*> dissolveTokens.stringLiteral)
  pure $ SiteB x y second

site = punt "Site" Site

distanceRange :: MyParser ModulePart
distanceRange = dissolveTokens.symbol "DistanceRange" *> (DistanceRange <$> dissolveTokens.float <*> dissolveTokens.float <*> dissolveTokens.float)

angleRange :: MyParser ModulePart
angleRange = dissolveTokens.symbol "AngleRange" *> (AngleRange <$> dissolveTokens.float <*> dissolveTokens.float <*> dissolveTokens.float)

excludeSameMolecule :: MyParser ModulePart
excludeSameMolecule = dissolveTokens.symbol "ExcludeSameMolecule" *> (ExcludeSameMolecule <$> ((dissolveTokens.symbol "On" *> pure true) <|> (dissolveTokens.identifier *> pure false)))

internalData1D = dissolveTokens.symbol "InternalData1D" *> (InternalData1D <$> dissolveTokens.stringLiteral <*> dissolveTokens.stringLiteral)

rangeX :: MyParser ModulePart
rangeX = dissolveTokens.symbol "RangeX" *> (RangeX <$> signedFloat <*> signedFloat <*> signedFloat)

rangeY :: MyParser ModulePart
rangeY = dissolveTokens.symbol "RangeY" *> (RangeY <$> signedFloat <*> signedFloat <*> signedFloat)

rangeZ :: MyParser ModulePart
rangeZ = dissolveTokens.symbol "RangeZ" *> (RangeZ <$> signedFloat <*> signedFloat <*> signedFloat)

range :: MyParser ModulePart
range = dissolveTokens.symbol "Range" *> (Range <$> dissolveTokens.float)

multiplicity :: MyParser ModulePart
multiplicity = dissolveTokens.symbol "Multiplicity" *> (Multiplicity <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer)

qMin :: MyParser ModulePart
qMin = dissolveTokens.symbol "QMin" *> (QMin <$> dissolveTokens.float)

qMax :: MyParser ModulePart
qMax = dissolveTokens.symbol "QMax" *> (QMax <$> dissolveTokens.float)

qDelta :: MyParser ModulePart
qDelta = dissolveTokens.symbol "QDelta" *> (QDelta <$> dissolveTokens.float)

qBroadening :: MyParser ModulePart
qBroadening = punt "QBroadening" QBroadening

windowFunction :: MyParser ModulePart
windowFunction = dissolveTokens.symbol "WindowFunction" *> (WindowFunction <$> dissolveTokens.identifier)

includeBragg :: MyParser ModulePart
includeBragg = dissolveTokens.symbol "IncludeBragg" *> (IncludeBragg <$> dissolveTokens.stringLiteral)

braggQBroadening :: MyParser ModulePart
braggQBroadening = dissolveTokens.symbol "BraggQBroadening" *> (BraggQBroadening <$> allString <*> signedFloat <*> signedFloat)

testReflections = dissolveTokens.symbol "TestReflections" *> (TestReflections <$> dissolveTokens.stringLiteral)

method = dissolveTokens.symbol "Method" *> (Method <$> dissolveTokens.identifier)

errorType = dissolveTokens.symbol "ErrorType" *> (ErrorType <$> dissolveTokens.identifier)

sourceRDF = dissolveTokens.symbol "SourceRDF" *> (SourceRDF <$> dissolveTokens.stringLiteral)

sourceRDFs = dissolveTokens.symbol "SourceRDFs" *> (SourceRDFs <$> dissolveTokens.stringLiteral)

sourceSQs :: MyParser ModulePart
sourceSQs = dissolveTokens.symbol "SourceSQs" *> (SourceSQs <$> dissolveTokens.stringLiteral)

threshold :: MyParser ModulePart
threshold = dissolveTokens.symbol "Threshold" *> (Threshold <$> dissolveTokens.float)

rangeA = dissolveTokens.symbol "RangeA" *> (RangeA <$> dissolveTokens.float <*> dissolveTokens.float)

rangeB = dissolveTokens.symbol "RangeB" *> (RangeB <$> dissolveTokens.float <*> dissolveTokens.float)

rangeBEnabled = dissolveTokens.symbol "RangeBEnabled" *> (RangeBEnabled <$> bool)

sampledDouble = dissolveTokens.symbol "SampledDouble" *> (SampledDouble <$> dissolveTokens.stringLiteral <*> dissolveTokens.float)

y_ = dissolveTokens.symbol "Y" *> (Y <$> dissolveTokens.integer)

xMin = dissolveTokens.symbol "XMin" *> (XMin <$> signedFloat)

data1DPart = y_ <|> xMin

data1D = sksContainer "Data1D" data1DPart Data1D <?> "Failed Data1D"

export = do
  _ <- pure 1
  namedValueContainer "Export" data1DPart Export

save = dissolveTokens.symbol "Save" *> (Save <$> dissolveTokens.identifier)

onlyWhenEnergyStable = dissolveTokens.symbol "OnlyWhenEnergyStable" *> (OnlyWhenEnergyStable <$> bool)

eReq = dissolveTokens.symbol "EReq" *> (EReq <$> signedFloat)

inpAFile = dissolveTokens.symbol "InpAFile" *> (InpAFile <$> dissolveTokens.stringLiteral)

nPItSs = dissolveTokens.symbol "NPItSs" *> (NPItSs <$> dissolveTokens.integer)

feedback = dissolveTokens.symbol "Feedback" *> (Feedback <$> signedFloat)

pCofFile = dissolveTokens.symbol "PCofFile" *> (PCofFile <$> dissolveTokens.stringLiteral)

referenceFTQMin = dissolveTokens.symbol "ReferenceFTQMin" *> (ReferenceFTQMin <$> signedFloat)

referenceFTQMax = dissolveTokens.symbol "ReferenceFTQMax" *> (ReferenceFTQMax <$> signedFloat)

referenceNormalisation = dissolveTokens.symbol "ReferenceNormalisation" *> (ReferenceNormalisation <$> allString)

referenceWindowFunction = dissolveTokens.symbol "ReferenceWindowFunction" *> (ReferenceWindowFunction <$> allString)

saveRepresentativeGR = dissolveTokens.symbol "SaveRepresentativeGR" *> (SaveRepresentativeGR <$> bool)

saveEstimatedPartials = dissolveTokens.symbol "SaveEstimatedPartials" *> (SaveEstimatedPartials <$> bool)

saveReference = dissolveTokens.symbol "SaveReference" *> (SaveReference <$> bool)

saveSQ = dissolveTokens.symbol "SaveSQ" *> (SaveSQ <$> bool)

overwritePotentials = dissolveTokens.symbol "OverwritePotentials" *> (OverwritePotentials <$> bool)

normalisation = dissolveTokens.symbol "Normalisation" *> (Normalisation <$> dissolveTokens.identifier)

test = dissolveTokens.symbol "Test" *> (Test <$> bool)

testAbsEnergyEP = dissolveTokens.symbol "TestAbsEnergyEP" *> (TestAbsEnergyEP <$> allString <*> dissolveTokens.float)

testAnalytic = dissolveTokens.symbol "TestAnalytic" *> (TestAnalytic <$> bool)

testReference = namedValueContainer "TestReference" data1DPart TestReference

testReferenceInter = punt "TestReferenceInter" TestReferenceInter

testReferenceIntra = dissolveTokens.symbol "TestReferenceIntra" *> (TestReferenceIntra <$> dissolveTokens.float)

testThreshold = dissolveTokens.symbol "TestThreshold" *> (TestThreshold <$> dissolveTokens.float)

species = dissolveTokens.symbol "Species" *> (Species <$> allString)

atomType = dissolveTokens.symbol "AtomType" *> (AtomType <$> dissolveTokens.integer <*> allString)

totalCharge = dissolveTokens.symbol "TotalCharge" *> (TotalCharge <$> signedNum)

bond = dissolveTokens.symbol "Bond" *> (Bond <$> dissolveTokens.integer <*> dissolveTokens.integer <*> signedNum <*> signedNum)

angle = dissolveTokens.symbol "Angle" *> (Angle <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> many signedNum)

torsion = dissolveTokens.symbol "Torsion" *> (Torsion <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> many signedNum)

improper = dissolveTokens.symbol "Improper" *> (Improper <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> many signedNum)

nSteps = dissolveTokens.symbol "NSteps" *> (NSteps <$> dissolveTokens.integer)

internalTest = dissolveTokens.symbol "InternalTest" *> (InternalTest <$> bool)

exchangeable = punt "Exchangeable" Exchangeable

expansionFunction = dissolveTokens.symbol "ExpansionFunction" *> (ExpansionFunction <$> dissolveTokens.identifier)

isotopologue = punt "Isotopologue" Isotopologue

sampledVector = do
  _ <- dissolveTokens.symbol "SampledVector"
  name <- dissolveTokens.stringLiteral
  kind <- dissolveTokens.identifier
  third <- dissolveTokens.symbol "@"
  _ <- dissolveTokens.symbol "EndSampledVector"
  pure $ SampledVector name kind third

reference = namedValueContainer "Reference" data1DPart Reference

analyser = container "Analyser" analyserPart Analyser

rawNum = RawNum <$> signedNum

modulePart = species <|> atomType <|> totalCharge <|> bond <|> angleRange <|> angle <|> torsion <|> improper <|> data1D <|> distanceRange <|> configuration <|> frequency <|> distance <|> format <|> binWidth <|> intraBroadening <|> averagingScheme <|> averaging <|> target <|> data_ <|> siteA <|> siteB <|> excludeSameMolecule <|> internalData1D <|> rangeBEnabled <|> rangeA <|> rangeB <|> rangeX <|> rangeY <|> rangeZ <|> range <|> multiplicity <|> qDelta <|> qMin <|> qMax <|> qBroadening <|> testReflections <|> method <|> sourceRDFs <|> sourceRDF <|> windowFunction <|> includeBragg <|> braggQBroadening <|> sampledDouble <|> sourceSQs <|> threshold <|> isotopologue <|> site <|> sampledVector <|> errorType <|> export <|> saveRepresentativeGR <|> saveEstimatedPartials <|> saveReference <|> saveSQ <|> save <|> eReq <|> inpAFile <|> nPItSs <|> feedback <|> referenceFTQMin <|> referenceFTQMax <|> referenceNormalisation <|> referenceWindowFunction <|> reference <|> pCofFile <|> onlyWhenEnergyStable <|> normalisation <|> overwritePotentials <|> testAbsEnergyEP <|> testAnalytic <|> testReferenceInter <|> testReferenceIntra <|> testThreshold <|> testReference <|> test <|> expansionFunction <|> nSteps <|> internalTest <|> exchangeable <|> analyser <|> rawNum <?> "Module Part"

module_ :: MyParser LayerPart
module_ = do
  terms <- punt "Module" identity
  contents <- many1Till modulePart $ string "End"
  _ <- dissolveTokens.reserved "Module"
  pure (Module terms $ toUnfoldable contents)

layerFrequency = dissolveTokens.symbol "Frequency" *> (LayerFrequency <$> dissolveTokens.integer)

layerPart = layerFrequency <|> module_

----------------------------------------------------------------------------
popOnLayer :: Array LayerPart -> Json -> Json
popOnLayer xs s = foldl go s xs
  where
  go s (Module terms parts) = updateArray "modules" (\c -> fromArray $ flip snoc (writeModule terms parts) c) s

  go s (LayerFrequency x) = "frequency" := x ~> s

writeModule terms = foldl go ("names" := terms ~> jsonEmptyObject)
  where
  go :: Json -> ModulePart -> Json
  go s (Configuration x) = "configuration" := x ~> s

  go s (Frequency x) = "frequency" := x ~> s

  go s (Distance i j dist) = "distance" := ("i" := i ~> "j" := j ~> "measure" := dist ~> jsonEmptyObject) ~> s

  go s (Format typ file) = "format" := ("type" := typ ~> "file" := file ~> jsonEmptyObject) ~> s

  go s (BinWidth x) = "binWidth" := x ~> s

  go s (IntraBroadening x) = "intraBroadening" := x ~> s

  go s (Averaging x) = "averaging" := x ~> s

  go s (AveragingScheme x) = "averagingScheme" := x ~> s

  go s (Target x) = "target" := x ~> s

  go s (Data x) = "data" := x ~> s

  go s (SiteA name site Nothing) = "siteA" := ("species" := name ~> "site" := site ~> jsonEmptyObject) ~> s

  go s (SiteA name site (Just (Tuple name2 site2))) = "siteA" := ("species" := name ~> "site" := site ~> "species2" := name2 ~> "site2" := site2 ~> jsonEmptyObject) ~> s

  go s (SiteB name site Nothing) = "siteB" := ("species" := name ~> "site" := site ~> jsonEmptyObject) ~> s

  go s (SiteB name site (Just (Tuple name2 site2))) = "siteB" := ("species" := name ~> "site" := site ~> "species2" := name2 ~> "site2" := site2 ~> jsonEmptyObject) ~> s

  go s (Site ss) = "site" := ss ~> s

  go s (DistanceRange x y z) = "distanceRange" := [ x, y, z ] ~> s

  go s (AngleRange x y z) = "angleRange" := [ x, y, z ] ~> s

  go s (ExcludeSameMolecule x) = "excludeSameMolecule" := x ~> s

  go s (RangeX low high step) = "rangeX" := ("low" := low ~> "high" := high ~> "step" := step ~> jsonEmptyObject) ~> s

  go s (RangeY low high step) = "rangeY" := ("low" := low ~> "high" := high ~> "step" := step ~> jsonEmptyObject) ~> s

  go s (RangeZ low high step) = "rangeZ" := ("low" := low ~> "high" := high ~> "step" := step ~> jsonEmptyObject) ~> s

  go s (Range x) = "range" := x ~> s

  go s (InternalData1D source dest) = "internalData1D" := ("source" := source ~> "dest" := dest ~> jsonEmptyObject) ~> s

  go s (Multiplicity x y z) = "multiplicity" := [ x, y, z ] ~> s

  go s (QBroadening qs) = "qBroadening" := qs ~> s

  go s (QDelta x) = "qDelta" := x ~> s

  go s (QMax x) = "qMax" := x ~> s

  go s (QMin x) = "qMin" := x ~> s

  go s (TestReflections x) = "testReflections" := x ~> s

  go s (Method x) = "method" := x ~> s

  go s (SourceRDF x) = "sourceRDF" := x ~> s

  go s (SourceRDFs x) = "sourceRDFs" := x ~> s

  go s (WindowFunction x) = "windowFunction" := x ~> s

  go s (IncludeBragg x) = "includeBragg" := x ~> s

  go s (BraggQBroadening typ i j) = "braggQBroadening" := ("type" := typ ~> "i" := i ~> "j" := j ~> jsonEmptyObject) ~> s

  go s (SourceSQs x) = "sourceSQs" := x ~> s

  -- FIXME: Include Data1D parts
  go s (Data1D loc kind file _) = "data1D" := ("location" := loc ~> "format" := kind ~> "file" := file ~> jsonEmptyObject) ~> s

  go s (Threshold x) = "threshold" := x ~> s

  go s (Isotopologue x) = "isotopologue" := x ~> s

  go s (SampledDouble loc value) = "sampledDouble" := ("location" := loc ~> "value" := value ~> jsonEmptyObject) ~> s

  go s (SampledVector loc value ref) = "sampledVector" := ("location" := loc ~> "value" := value ~> "ref" := ref ~> jsonEmptyObject) ~> s

  go s (ErrorType x) = "errorType" := x ~> s

  go s (Normalisation x) = "normalisation" := x ~> s

  go s (RangeA low high) = "rangeA" := ("low" := low ~> "high" := high ~> jsonEmptyObject) ~> s

  go s (RangeB low high) = "rangeB" := ("low" := low ~> "high" := high ~> jsonEmptyObject) ~> s

  go s (RangeBEnabled x) = "rangeBEnabled" := x ~> s

  go s (Test x) = "test" := x ~> s

  go s (TestAbsEnergyEP name value) = updateInner "testAbsEnergyEP" (\c -> name := value ~> c) s

  go s (TestAnalytic x) = "testAnalytic" := x ~> s

  go s (TestReferenceInter xs) = "testReferenceInter" := xs ~> s

  go s (TestReferenceIntra x) = "testReferenceIntra" := x ~> s

  -- FIXME: Handle children
  go s (TestReference name path _) = "testReference" := ("name" := name ~> "path" := path ~> jsonEmptyObject) ~> s

  go s (TestThreshold x) = "testThreshold" := x ~> s

  go s (Save x) = "save" := x ~> s

  go s (EReq x) = "eReq" := x ~> s

  go s (InpAFile x) = "inpAFile" := x ~> s

  go s (NPItSs x) = "nPItSs" := x ~> s

  go s (Feedback x) = "feedback" := x ~> s

  go s (Reference name value items) = updateArray "references" (fromArray <<< flip snoc ("name" := name ~> "value" := value ~> jsonEmptyObject)) s

  go s (Exchangeable x) = "exchangeable" := x ~> s

  go s (ExpansionFunction x) = "expansionFunction" := x ~> s

  go s (PCofFile x) = "pCofFile" := x ~> s

  go s (OnlyWhenEnergyStable x) = "onlyWhenEnergyStable" := x ~> s

  go s (ReferenceFTQMin x) = "referenceFTQMin" := x ~> s

  go s (ReferenceFTQMax x) = "referenceFTQMax" := x ~> s

  go s (ReferenceNormalisation x) = "referenceNormalisation" := x ~> s

  go s (ReferenceWindowFunction x) = "referenceWindowFunction" := x ~> s

  go s (SaveRepresentativeGR x) = "saveRepresentativeGR" := x ~> s

  go s (SaveEstimatedPartials x) = "saveEstimatedPartials" := x ~> s

  go s (SaveReference x) = "saveReference" := x ~> s

  go s (SaveSQ x) = "saveSQ" := x ~> s

  go s (Species x) = "species" := x ~> s

  go s (AtomType idx name) = updateArray "atomTypes" (fromArray <<< flip snoc ("type" := name ~> "index" := idx ~> jsonEmptyObject)) s

  go s (TotalCharge x) = "totalCharge" := x ~> s

  go s (NSteps x) = "nSteps" := x ~> s

  go s (InternalTest x) = "internalTest" := x ~> s

  go s (Bond i j const eq) = updateArray "bonds" (fromArray <<< flip snoc ("eq" := eq ~> "const" := const ~> "j" := j ~> "i" := i ~> jsonEmptyObject)) s

  go s (Angle i j k terms) = updateArray "angles" (fromArray <<< flip snoc ("terms" := terms ~> "k" := k ~> "j" := j ~> "i" := i ~> jsonEmptyObject)) s

  go s (Torsion i j k l terms) = updateArray "torsions" (fromArray <<< flip snoc ("terms" := terms ~> "l" := l ~> "k" := k ~> "j" := j ~> "i" := i ~> jsonEmptyObject)) s

  go s (Improper i j k l terms) = updateArray "impropers" (fromArray <<< flip snoc ("terms" := terms ~> "l" := l ~> "k" := k ~> "j" := j ~> "i" := i ~> jsonEmptyObject)) s

  go s (OverwritePotentials x) = "overwritePotentials" := x ~> s

  go s (RawNum x) = updateArray "rawNumbers" (fromArray <<< flip snoc (encodeJson x)) s

  -- FIXME: Handle children
  go s (Export format path _) = "export" := ("format" := format ~> "path" := path ~> jsonEmptyObject) ~> s

  -- FIXME: Handle Analysers
  go s (Analyser parts) = updateArray "analyser" (\c -> fromArray $ flip snoc (encodeJson 7) c) s
