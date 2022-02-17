module Analyser where

import Prelude
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
import Util (MyParser, container, dissolveTokens, namedContainer, namedValueContainer, punt, signedFloat, signedNum, sksContainer)

data AnalyserPart
  = Site (Array String)
  | Select String (Array AnalyserPart)
  | ForEach (Array AnalyserPart)
  | ExcludeSameMolecule String
  | CalculateDistance String (Array AnalyserPart)
  | CalculateAngle String (Array AnalyserPart)
  | I String
  | J String
  | K String
  | L String
  | Collect1D String (Array AnalyserPart)
  | Collect2D String (Array AnalyserPart)
  | SubCollect (Array AnalyserPart)
  | QuantityX String
  | QuantityY String
  | RangeX Number Number Number
  | RangeY Number Number Number
  | Process1D String (Array AnalyserPart)
  | Process2D String (Array AnalyserPart)
  | SourceData String
  | LabelValue String
  | LabelX String
  | LabelY String
  | Normalisation (Array AnalyserPart)
  | OperateSitePopulationNormalise (Array AnalyserPart)
  | OperateNumberDensityNormalise (Array AnalyserPart)
  | OperateSphericalShellNormalise (Array AnalyserPart)
  | DynamicSite (Array AnalyserPart)
  | Element String
  | SameMoleculeAsSite String
  | OperateExpression (Array AnalyserPart)
  | Expression String
  | OperateNormalise (Array AnalyserPart)
  | Value Number

derive instance genericAnalyserPart :: Generic AnalyserPart _

instance showAnalyserPart :: Show AnalyserPart where
  show x = genericShow x

site = punt "Site" Site

select = do
  _ <- pure 1
  namedContainer "Select" analyserPart Select

forEach = do
  _ <- pure 1
  container "ForEach" analyserPart ForEach

excludeSameMolecule = dissolveTokens.symbol "ExcludeSameMolecule" *> (ExcludeSameMolecule <$> dissolveTokens.stringLiteral)

calculateDistance = do
  _ <- pure 1
  namedContainer "CalculateDistance" analyserPart CalculateDistance

calculateAngle = do
  _ <- pure 1
  namedContainer "CalculateAngle" analyserPart CalculateAngle

collect1D = do
  _ <- pure 1
  namedContainer "Collect1D" analyserPart Collect1D

collect2D = do
  _ <- pure 1
  namedContainer "Collect2D" analyserPart Collect2D

subCollect = do
  _ <- pure 1
  container "SubCollect" analyserPart SubCollect

i = dissolveTokens.symbol "I" *> (I <$> dissolveTokens.stringLiteral)

j = dissolveTokens.symbol "J" *> (J <$> dissolveTokens.stringLiteral)

k = dissolveTokens.symbol "K" *> (K <$> dissolveTokens.stringLiteral)

l = dissolveTokens.symbol "L" *> (L <$> dissolveTokens.stringLiteral)

quantityX = dissolveTokens.symbol "QuantityX" *> (QuantityX <$> dissolveTokens.stringLiteral)

quantityY = dissolveTokens.symbol "QuantityY" *> (QuantityY <$> dissolveTokens.stringLiteral)

rangeX = dissolveTokens.symbol "RangeX" *> (RangeX <$> dissolveTokens.float <*> dissolveTokens.float <*> dissolveTokens.float)

rangeY = dissolveTokens.symbol "RangeY" *> (RangeY <$> dissolveTokens.float <*> dissolveTokens.float <*> dissolveTokens.float)

process1D = do
  _ <- pure 1
  namedContainer "Process1D" analyserPart Process1D

process2D = do
  _ <- pure 2
  namedContainer "Process2D" analyserPart Process2D

labelValue = dissolveTokens.symbol "LabelValue" *> (LabelValue <$> dissolveTokens.stringLiteral)

sourceData = dissolveTokens.symbol "SourceData" *> (SourceData <$> dissolveTokens.stringLiteral)

labelX = dissolveTokens.symbol "LabelX" *> (LabelX <$> dissolveTokens.stringLiteral)

labelY = dissolveTokens.symbol "LabelY" *> (LabelY <$> dissolveTokens.stringLiteral)

normalisation = do
  _ <- pure 1
  container "Normalisation" analyserPart Normalisation

operateSitePopulationNormalise = do
  _ <- pure 1
  container "OperateSitePopulationNormalise" analyserPart OperateSitePopulationNormalise

operateNumberDensityNormalise = do
  _ <- pure 1
  container "OperateNumberDensityNormalise" analyserPart OperateNumberDensityNormalise

operateSphericalShellNormalise = do
  _ <- pure 1
  container "OperateSphericalShellNormalise" analyserPart OperateSphericalShellNormalise

dynamicSite = do
  _ <- pure 1
  container "DynamicSite" analyserPart DynamicSite

element = dissolveTokens.symbol "Element" *> (Element <$> dissolveTokens.stringLiteral)

sameMoleculeAsSite = dissolveTokens.symbol "SameMoleculeAsSite" *> (SameMoleculeAsSite <$> dissolveTokens.stringLiteral)

operateExpression = do
  _ <- pure 1
  container "OperateExpression" analyserPart OperateExpression

operateNormalise = do
  _ <- pure 1
  container "OperateNormalise" analyserPart OperateNormalise

expression = dissolveTokens.symbol "Expression" *> (Expression <$> dissolveTokens.stringLiteral)

value = dissolveTokens.symbol "Value" *> (Value <$> signedFloat)

analyserPart = do
  _ <- pure 1
  site <|> select <|> forEach <|> excludeSameMolecule <|> calculateDistance <|> calculateAngle <|> collect1D <|> collect2D <|> subCollect <|> quantityX <|> quantityY <|> rangeX <|> rangeY <|> process1D <|> process2D <|> labelValue <|> sourceData <|> labelX <|> labelY <|> normalisation <|> operateSitePopulationNormalise <|> operateNumberDensityNormalise <|> operateSphericalShellNormalise <|> dynamicSite <|> element <|> sameMoleculeAsSite <|> operateExpression <|> operateNormalise <|> expression <|> value <|> i <|> j <|> k <|> l <?> "Procedure Node"
