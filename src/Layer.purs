module Layer where

import Prelude
import Util (dissolveTokens, signedFloat)
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (sepBy1, many1Till)
import Text.Parsing.Parser.String (skipSpaces, string)
import Data.List.NonEmpty (toUnfoldable)

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

modulePart = configuration <|> frequency <|> distance <|> angle

layerPart :: Parser String LayerPart
layerPart = do
  _ <- dissolveTokens.reserved "Module"
  name <- dissolveTokens.identifier
  contents <- many1Till modulePart $ string "End"
  _ <- dissolveTokens.reserved "Module"
  pure (Module name $ toUnfoldable contents)
