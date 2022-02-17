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
import Util (MyParser, container, dissolveTokens, namedContainer, punt, signedFloat, signedNum, sksContainer)

data AnalyserPart
  = Site (Array String)
  | Select String (Array AnalyserPart)
  | ForEach (Array AnalyserPart)
  | ExcludeSameMolecule String
  | CalculateDistance String (Array AnalyserPart)
  | I String
  | J String
  | Collect1D String (Array AnalyserPart)
  | QuantityX String
  | RangeX Number Number Number
  | Process1D String (Array AnalyserPart)
  | SourceData String
  | LabelValue String
  | LabelX String
  | Normalisation (Array AnalyserPart)
  | OperateSitePopulationNormalise (Array AnalyserPart)
  | OperateNumberDensityNormalise (Array AnalyserPart)
  | OperateSphericalShellNormalise (Array AnalyserPart)

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

collect1D = do
  _ <- pure 1
  namedContainer "Collect1D" analyserPart Collect1D

i = dissolveTokens.symbol "I" *> (I <$> dissolveTokens.stringLiteral)

j = dissolveTokens.symbol "J" *> (J <$> dissolveTokens.stringLiteral)

quantityX = dissolveTokens.symbol "QuantityX" *> (QuantityX <$> dissolveTokens.stringLiteral)

rangeX = dissolveTokens.symbol "RangeX" *> (RangeX <$> dissolveTokens.float <*> dissolveTokens.float <*> dissolveTokens.float)

process1D = do
  _ <- pure 1
  namedContainer "Process1D" analyserPart Process1D

labelValue = dissolveTokens.symbol "LabelValue" *> (LabelValue <$> dissolveTokens.stringLiteral)

sourceData = dissolveTokens.symbol "SourceData" *> (SourceData <$> dissolveTokens.stringLiteral)

labelX = dissolveTokens.symbol "LabelX" *> (LabelX <$> dissolveTokens.stringLiteral)

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

analyserPart = do
  _ <- pure 1
  site <|> select <|> forEach <|> excludeSameMolecule <|> calculateDistance <|> collect1D <|> quantityX <|> rangeX <|> process1D <|> labelValue <|> sourceData <|> labelX <|> normalisation <|> operateSitePopulationNormalise <|> operateNumberDensityNormalise <|> operateSphericalShellNormalise <|> i <|> j
