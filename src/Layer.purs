module Layer where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (many1Till, optionMaybe)
import Text.Parsing.Parser.String (string)
import Util (dissolveTokens)

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
  | DistanceRange Number Number Number
  | ExcludeSameMolecule Boolean
  | InternalData1D String String
  | Range Number
  | Multiplicity Int Int Int
  | QBroadening Number
  | QMax Number
  | QMin Number
  | TestReflections String
  | Method String
  | SourceRDFs String

derive instance genericModulePart :: Generic ModulePart _

instance showModulePart :: Show ModulePart where
  show x = genericShow x

configuration :: Parser String ModulePart
configuration = (dissolveTokens.symbol "Configurations" <|> dissolveTokens.symbol "Configuration") *> (Configuration <$> dissolveTokens.stringLiteral)

frequency :: Parser String ModulePart
frequency = dissolveTokens.symbol "Frequency" *> (Frequency <$> dissolveTokens.integer)

distance :: Parser String ModulePart
distance = dissolveTokens.symbol "Distance" *> (Distance <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.float)

angle :: Parser String ModulePart
angle = dissolveTokens.symbol "Angle" *> (Angle <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.float)

format :: Parser String ModulePart
format = do
  _ <- dissolveTokens.reserved "Format"
  kind <- dissolveTokens.identifier
  file <- dissolveTokens.stringLiteral
  -- contents <- many1Till modulePart $ string "End"
  _ <- dissolveTokens.reserved "EndFormat"
  pure (Format kind file)

binWidth :: Parser String ModulePart
binWidth = dissolveTokens.symbol "BinWidth" *> (BinWidth <$> dissolveTokens.float)

intraBroadening :: Parser String ModulePart
intraBroadening = dissolveTokens.symbol "IntraBroadening" *> (IntraBoradening <$> dissolveTokens.identifier)

averaging :: Parser String ModulePart
averaging = dissolveTokens.symbol "Averaging" *> (Averaging <$> dissolveTokens.integer)

target :: Parser String ModulePart
target = dissolveTokens.symbol "Target" *> (Target <$> dissolveTokens.stringLiteral)

data_ :: Parser String ModulePart
data_ = dissolveTokens.symbol "Data" *> (Data <$> dissolveTokens.stringLiteral)

siteA :: Parser String ModulePart
siteA = do
  _ <- dissolveTokens.symbol "SiteA"
  x <- dissolveTokens.identifier
  y <- dissolveTokens.stringLiteral
  second <- optionMaybe (Tuple <$> dissolveTokens.reserved x <*> dissolveTokens.stringLiteral)
  pure $ SiteA x y Nothing

siteB :: Parser String ModulePart
siteB = do
  _ <- dissolveTokens.symbol "SiteB"
  x <- dissolveTokens.identifier
  y <- dissolveTokens.stringLiteral
  second <- optionMaybe (Tuple <$> dissolveTokens.reserved x <*> dissolveTokens.stringLiteral)
  pure $ SiteB x y Nothing

distanceRange :: Parser String ModulePart
distanceRange = dissolveTokens.symbol "DistanceRange" *> (DistanceRange <$> dissolveTokens.float <*> dissolveTokens.float <*> dissolveTokens.float)

excludeSameMolecule :: Parser String ModulePart
excludeSameMolecule = dissolveTokens.symbol "ExcludeSameMolecule" *> (ExcludeSameMolecule <$> ((dissolveTokens.symbol "On" *> pure true) <|> (dissolveTokens.identifier *> pure false)))

internalData1D = dissolveTokens.symbol "InternalData1D" *> (InternalData1D <$> dissolveTokens.stringLiteral <*> dissolveTokens.stringLiteral)

range :: Parser String ModulePart
range = dissolveTokens.symbol "Range" *> (Range <$> dissolveTokens.float)

multiplicity :: Parser String ModulePart
multiplicity = dissolveTokens.symbol "Multiplicity" *> (Multiplicity <$> dissolveTokens.integer <*> dissolveTokens.integer <*> dissolveTokens.integer)

qMin :: Parser String ModulePart
qMin = dissolveTokens.symbol "QMin" *> (QMin <$> dissolveTokens.float)

qMax :: Parser String ModulePart
qMax = dissolveTokens.symbol "QMax" *> (QMax <$> dissolveTokens.float)

qBroadening :: Parser String ModulePart
qBroadening = dissolveTokens.symbol "QBroadening" *> (QBroadening <$> dissolveTokens.float)

testReflections = dissolveTokens.symbol "TestReflections" *> (TestReflections <$> dissolveTokens.stringLiteral)

method = dissolveTokens.symbol "Method" *> (Method <$> dissolveTokens.identifier)

sourceRDFs = dissolveTokens.symbol "SourceRDFs" *> (SourceRDFs <$> dissolveTokens.stringLiteral)

modulePart = distanceRange <|> configuration <|> frequency <|> distance <|> angle <|> format <|> binWidth <|> intraBroadening <|> averaging <|> target <|> data_ <|> siteA <|> siteB <|> excludeSameMolecule <|> internalData1D <|> range <|> multiplicity <|> qMin <|> qMax <|> qBroadening <|> testReflections <|> method <|> sourceRDFs

layerPart :: Parser String LayerPart
layerPart = do
  _ <- dissolveTokens.reserved "Module"
  kind <- dissolveTokens.identifier
  name <- optionMaybe $ dissolveTokens.stringLiteral
  contents <- many1Till modulePart $ string "End"
  _ <- dissolveTokens.reserved "Module"
  pure (Module kind name $ toUnfoldable contents)
