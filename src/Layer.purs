module Layer where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (toUnfoldable)
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (sepBy1, many1Till)
import Text.Parsing.Parser.String (skipSpaces, string)
import Util (dissolveTokens, signedFloat)

data LayerPart
  = Module String (Array ModulePart)

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
  | SiteA String String
  | SiteB String String
  | DistanceRange Number Number Number
  | ExcludeSameMolecule Boolean
  | InternalData1D String String

derive instance genericModulePart :: Generic ModulePart _

instance showModulePart :: Show ModulePart where
  show x = genericShow x

configuration :: Parser String ModulePart
configuration = dissolveTokens.symbol "Configuration" *> (Configuration <$> dissolveTokens.stringLiteral)

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
siteA = dissolveTokens.symbol "SiteA" *> (SiteA <$> dissolveTokens.identifier <*> dissolveTokens.stringLiteral)

siteB :: Parser String ModulePart
siteB = dissolveTokens.symbol "SiteB" *> (SiteB <$> dissolveTokens.identifier <*> dissolveTokens.stringLiteral)

distanceRange :: Parser String ModulePart
distanceRange = dissolveTokens.symbol "DistanceRange" *> (DistanceRange <$> dissolveTokens.float <*> dissolveTokens.float <*> dissolveTokens.float)

excludeSameMolecule :: Parser String ModulePart
excludeSameMolecule = dissolveTokens.symbol "ExcludeSameMolecule" *> (ExcludeSameMolecule <$> ((dissolveTokens.symbol "On" *> pure true) <|> (dissolveTokens.identifier *> pure false)))

internalData1D = dissolveTokens.symbol "InternalData1D" *> (InternalData1D <$> dissolveTokens.stringLiteral <*> dissolveTokens.stringLiteral)

modulePart = configuration <|> frequency <|> distance <|> angle <|> format <|> binWidth <|> intraBroadening <|> averaging <|> target <|> data_ <|> siteA <|> siteB <|> distanceRange <|> excludeSameMolecule <|> internalData1D

layerPart :: Parser String LayerPart
layerPart = do
  _ <- dissolveTokens.reserved "Module"
  name <- dissolveTokens.identifier
  contents <- many1Till modulePart $ string "End"
  _ <- dissolveTokens.reserved "Module"
  pure (Module name $ toUnfoldable contents)
