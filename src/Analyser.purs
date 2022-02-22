module Analyser where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Prelude
import Xml
import Control.Alternative ((<|>))
import Data.Array (foldl, many)
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

---------
writeAnalyser :: Array AnalyserPart -> Json
writeAnalyser = foldl go jsonEmptyObject
  where
  go s (Site xs) = "site" := xs ~> s

  go s (Select name parts) = "select" := (writeAnalyser parts) ~> s

  go s (ForEach parts) = "forEach" := (writeAnalyser parts) ~> s

  go s (ExcludeSameMolecule x) = "excludeSameMolecule" := x ~> s

  go s (CalculateDistance name parts) = "calculateDistance" := ("name" := name ~> writeAnalyser parts) ~> s

  go s (CalculateAngle name parts) = "calculateAngle" := ("name" := name ~> writeAnalyser parts) ~> s

  go s (I x) = "I" := x ~> s

  go s (J x) = "J" := x ~> s

  go s (K x) = "K" := x ~> s

  go s (L x) = "L" := x ~> s

  go s (Collect1D name parts) = "collect1D" := ("name" := name ~> writeAnalyser parts) ~> s

  go s (Collect2D name parts) = "collect2D" := ("name" := name ~> writeAnalyser parts) ~> s

  go s (SubCollect parts) = "subcollect" := writeAnalyser parts ~> s

  go s (QuantityX x) = "quantityX" := x ~> s

  go s (QuantityY x) = "quantityY" := x ~> s

  go s (RangeX low high step) = "rangeX" := ("low" := low ~> "high" := high ~> "step" := step ~> jsonEmptyObject) ~> s

  go s (RangeY low high step) = "rangeY" := ("low" := low ~> "high" := high ~> "step" := step ~> jsonEmptyObject) ~> s

  go s (Process1D name parts) = "process1D" := ("name" := name ~> writeAnalyser parts) ~> s

  go s (Process2D name parts) = "process2D" := ("name" := name ~> writeAnalyser parts) ~> s

  go s (SourceData x) = "sourceData" := x ~> s

  go s (LabelValue x) = "labelValue" := x ~> s

  go s (LabelX x) = "labelX" := x ~> s

  go s (LabelY x) = "labelY" := x ~> s

  go s (Normalisation parts) = "normalisation" := writeAnalyser parts ~> s

  go s (OperateSitePopulationNormalise parts) = "operateSitePopulationNormalise" := writeAnalyser parts ~> s

  go s (OperateNumberDensityNormalise parts) = "operateNumberDensityNormalise" := writeAnalyser parts ~> s

  go s (OperateSphericalShellNormalise parts) = "operateSphericalShellNormalise" := writeAnalyser parts ~> s

  go s (DynamicSite parts) = "dynamicSite" := writeAnalyser parts ~> s

  go s (SameMoleculeAsSite x) = "sameMoleculeAsSite" := x ~> s

  go s (Element x) = "element" := x ~> s

  go s (OperateExpression parts) = "operateExpression" := writeAnalyser parts ~> s

  go s (OperateNormalise parts) = "operateNormalise" := writeAnalyser parts ~> s

  go s (Expression x) = "expression" := x ~> s

  go s (Value x) = "value" := x ~> s

xmlAnalyser :: AnalyserPart -> XmlNode -> XmlNode
xmlAnalyser (Site xs) = addChild $ addTerms "site" xs

xmlAnalyser (Select name parts) = addChild $ xmlActOn "select" $ [ "name" ::= name ] <> map xmlAnalyser parts

xmlAnalyser (ForEach parts) = addChild $ xmlActOn "forEach" $ map xmlAnalyser parts

xmlAnalyser (ExcludeSameMolecule x) = "excludeSameMolecule" ::= x

xmlAnalyser (CalculateDistance name parts) = addChild $ xmlActOn "calculateDistance" $ [ "name" ::= name ] <> map xmlAnalyser parts

xmlAnalyser (CalculateAngle name parts) = addChild $ xmlActOn "calculateAngle" $ [ "name" ::= name ] <> map xmlAnalyser parts

xmlAnalyser (I x) = "i" ::= x

xmlAnalyser (J x) = "j" ::= x

xmlAnalyser (K x) = "k" ::= x

xmlAnalyser (L x) = "l" ::= x

xmlAnalyser (Collect1D name parts) = addChild $ xmlActOn "collect1D" $ [ "name" ::= name ] <> map xmlAnalyser parts

xmlAnalyser (Collect2D name parts) = addChild $ xmlActOn "collect2D" $ [ "name" ::= name ] <> map xmlAnalyser parts

xmlAnalyser (SubCollect parts) = addChild $ xmlActOn "subCollect" $ map xmlAnalyser parts

xmlAnalyser (QuantityX x) = "quantityX" ::= x

xmlAnalyser (QuantityY x) = "quantityY" ::= x

xmlAnalyser (RangeX low high step) = addChild $ xmlActOn "rangeX" [ "low" ::= low, "high" ::= high, "step" ::= step ]

xmlAnalyser (RangeY low high step) = addChild $ xmlActOn "rangeY" [ "low" ::= low, "high" ::= high, "step" ::= step ]

xmlAnalyser (Process1D name parts) = addChild $ xmlActOn "process1D" $ [ "name" ::= name ] <> map xmlAnalyser parts

xmlAnalyser (Process2D name parts) = addChild $ xmlActOn "process2D" $ [ "name" ::= name ] <> map xmlAnalyser parts

xmlAnalyser (SourceData x) = "sourceData" ::= x

xmlAnalyser (LabelValue x) = "labelValue" ::= x

xmlAnalyser (LabelX x) = "labelX" ::= x

xmlAnalyser (LabelY x) = "labelY" ::= x

xmlAnalyser (Normalisation parts) = addChild $ xmlActOn "normalisation" $ map xmlAnalyser parts

xmlAnalyser (OperateSitePopulationNormalise parts) = addChild $ xmlActOn "operateSitePopulationNormalise" $ map xmlAnalyser parts

xmlAnalyser (OperateNumberDensityNormalise parts) = addChild $ xmlActOn "operateNumberDensityNormalise" $ map xmlAnalyser parts

xmlAnalyser (OperateSphericalShellNormalise parts) = addChild $ xmlActOn "operateSphericalShellNormalise" $ map xmlAnalyser parts

xmlAnalyser (DynamicSite parts) = addChild $ xmlActOn "dynamicSite" $ map xmlAnalyser parts

xmlAnalyser (OperateExpression parts) = addChild $ xmlActOn "operateExpression" $ map xmlAnalyser parts

xmlAnalyser (OperateNormalise parts) = addChild $ xmlActOn "operateNormalise" $ map xmlAnalyser parts

xmlAnalyser (Element x) = "element" ::= x

xmlAnalyser (SameMoleculeAsSite x) = "sameMoleculeAsSite" ::= x

xmlAnalyser (Expression x) = "expression" ::= x

xmlAnalyser (Value x) = "value" ::= x
