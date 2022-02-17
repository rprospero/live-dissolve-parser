module Layer where

import Prelude
import Analyser (analyserPart, AnalyserPart)
import Control.Alternative ((<|>))
import Data.Array (many)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser.Combinators (between, many1Till, optional, optionMaybe, skipMany, try, (<?>))
import Text.Parsing.Parser.String (char, satisfy, string, whiteSpace)
import Util (bool, container, dissolveTokens, MyParser, punt, signedFloat, signedNum, sksContainer)

data LayerPart
  = Module String (Maybe String) (Array ModulePart)

derive instance genericLayerPart :: Generic LayerPart _

instance showLayerPart :: Show LayerPart where
  show x = genericShow x

data ModulePart
  = Configuration String
  | Frequency Int
  | Distance Int Int Number
  | Angle Int Int Int Number
  | Format String String
  | BinWidth Number
  | IntraBoradening String
  | Averaging Int
  | Target String
  | Data String
  | SiteA String String (Maybe (Tuple String String))
  | SiteB String String (Maybe (Tuple String String))
  | Site (Array String)
  | DistanceRange Number Number Number
  | ExcludeSameMolecule Boolean
  | InternalData1D String String
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
  | Analyser (Array AnalyserPart)
  | RawNum (Either Int Number)

derive instance genericModulePart :: Generic ModulePart _

instance showModulePart :: Show ModulePart where
  show x = genericShow x

data Data1DPart
  = Y Int

derive instance genericData1Part :: Generic Data1DPart _

instance showData1DPart :: Show Data1DPart where
  show x = genericShow x

configuration :: MyParser ModulePart
configuration = (dissolveTokens.symbol "Configurations" <|> dissolveTokens.symbol "Configuration") *> (Configuration <$> dissolveTokens.stringLiteral)

frequency :: MyParser ModulePart
frequency = dissolveTokens.symbol "Frequency" *> (Frequency <$> dissolveTokens.integer)

distance :: MyParser ModulePart
distance = dissolveTokens.symbol "Distance" *> (Distance <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.float)

angle :: MyParser ModulePart
angle = dissolveTokens.symbol "Angle" *> (Angle <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.float)

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
intraBroadening = dissolveTokens.symbol "IntraBroadening" *> (IntraBoradening <$> dissolveTokens.identifier)

averaging :: MyParser ModulePart
averaging = dissolveTokens.symbol "Averaging" *> (Averaging <$> dissolveTokens.integer)

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

excludeSameMolecule :: MyParser ModulePart
excludeSameMolecule = dissolveTokens.symbol "ExcludeSameMolecule" *> (ExcludeSameMolecule <$> ((dissolveTokens.symbol "On" *> pure true) <|> (dissolveTokens.identifier *> pure false)))

internalData1D = dissolveTokens.symbol "InternalData1D" *> (InternalData1D <$> dissolveTokens.stringLiteral <*> dissolveTokens.stringLiteral)

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
braggQBroadening = dissolveTokens.symbol "BraggQBroadening" *> (BraggQBroadening <$> dissolveTokens.identifier <*> signedFloat <*> signedFloat)

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

data1DPart = y_

data1D = sksContainer "Data1D" data1DPart Data1D <?> "Failed Data1D"

isotopologue = punt "Isotopologue" Isotopologue

sampledVector = do
  _ <- dissolveTokens.symbol "SampledVector"
  name <- dissolveTokens.stringLiteral
  kind <- dissolveTokens.identifier
  third <- dissolveTokens.symbol "@"
  _ <- dissolveTokens.symbol "EndSampledVector"
  pure $ SampledVector name kind third

analyser = container "Analyser" analyserPart Analyser

rawNum = RawNum <$> signedNum

modulePart = data1D <|> distanceRange <|> configuration <|> frequency <|> distance <|> angle <|> format <|> binWidth <|> intraBroadening <|> averaging <|> target <|> data_ <|> siteA <|> siteB <|> excludeSameMolecule <|> internalData1D <|> rangeBEnabled <|> rangeA <|> rangeB <|> range <|> multiplicity <|> qDelta <|> qMin <|> qMax <|> qBroadening <|> testReflections <|> method <|> sourceRDFs <|> sourceRDF <|> windowFunction <|> includeBragg <|> braggQBroadening <|> sampledDouble <|> sourceSQs <|> threshold <|> isotopologue <|> site <|> sampledVector <|> errorType <|> analyser <|> rawNum

layerPart :: MyParser LayerPart
layerPart = do
  _ <- dissolveTokens.reserved "Module"
  _ <- many (char ' ')
  kind <- SCU.fromCharArray <$> many (satisfy (\c -> (c /= ' ') && (c /= '\n')))
  name <-
    optionMaybe do
      skipMany (satisfy (\c -> c == ' '))
      between (optional $ char '\'') (optional $ char '\'') $ SCU.fromCharArray <$> many (satisfy (\c -> (c /= ' ') && (c /= '\n')))
  _ <- char '\n'
  dissolveTokens.whiteSpace
  contents <- many1Till modulePart $ string "End"
  _ <- dissolveTokens.reserved "Module"
  pure (Module kind name $ toUnfoldable contents)
