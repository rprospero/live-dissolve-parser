module Configuration where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Foreign.Object
import Prelude hiding (between)
import Util
import Control.Alternative ((<|>))
import Data.Array (foldl, many)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.String (noneOf, skipSpaces, char, string)

data ConfigurationPart
  = Generator (Array GeneratorPart)
  | Temperature Number
  | InputCoordinates String String

derive instance genericConfigurationPart :: Generic ConfigurationPart _

instance showConfigurationPart :: Show ConfigurationPart where
  show x = genericShow x

data GeneratorPart
  = Add (Array GeneratorAddPart)
  | Box (Array BoxPart)
  | Parameters (Array Param)

derive instance genericGeneratorPart :: Generic GeneratorPart _

instance showGeneratorPart :: Show GeneratorPart where
  show x = genericShow x

data GeneratorAddPart
  = Density String String
  | Population Int
  | Species String
  | BoxAction String
  | Rotate Boolean
  | Positioning String

derive instance genericGeneratorAddPart :: Generic GeneratorAddPart _

instance showGeneratorAddPart :: Show GeneratorAddPart where
  show x = genericShow x

data BoxPart
  = Length Number Number Number
  | Angles Number Number Number
  | NonPeriodic Boolean

derive instance genericBoxPart :: Generic BoxPart _

instance showBoxPart :: Show BoxPart where
  show x = genericShow x

data Param
  = Param (Array String)

derive instance genericParam :: Generic Param _

instance showParam :: Show Param where
  show x = genericShow x

density = dissolveTokens.symbol "Density" *> (Density <$> arbitrary <*> dissolveTokens.symbol "atoms/A3")

population = dissolveTokens.symbol "Population" *> (Population <$> myInt)
  where
  myInt = ((between (char '\'') (char '\'') dissolveTokens.integer) <* dissolveTokens.whiteSpace) <|> dissolveTokens.integer

species = dissolveTokens.symbol "Species" *> (Species <$> dissolveTokens.stringLiteral)

boxAction = dissolveTokens.symbol "BoxAction" *> (BoxAction <$> dissolveTokens.identifier)

rotate = dissolveTokens.symbol "Rotate" *> (Rotate <$> bool)

positioning = dissolveTokens.symbol "Positioning" *> (Positioning <$> dissolveTokens.identifier)

generatorAddPart = density <|> population <|> species <|> boxAction <|> rotate <|> positioning

add = container "Add" generatorAddPart Add

length = dissolveTokens.symbol "Lengths" *> (Length <$> signedNum <*> signedNum <*> signedNum)

angles = dissolveTokens.symbol "Angles" *> (Angles <$> signedNum <*> signedNum <*> signedNum)

nonPeriodic = dissolveTokens.symbol "NonPeriodic" *> (NonPeriodic <$> bool)

boxPart = length <|> angles <|> nonPeriodic

box = container "Box" boxPart Box

param = punt "Parameter" Param

parameters = container "Parameters" param Parameters

generatorPart = add <|> box <|> parameters

generator = container "Generator" generatorPart Generator

temperature = dissolveTokens.symbol "Temperature" *> (Temperature <$> dissolveTokens.float)

inputCoordinates = do
  _ <- dissolveTokens.symbol "InputCoordinates"
  name <- dissolveTokens.identifier
  path <- many $ noneOf [ ' ', '\n', '\t' ]
  skipSpaces
  _ <- string "End"
  _ <- dissolveTokens.symbol "InputCoordinates"
  pure (InputCoordinates name $ fromCharArray path)

configurationPart = temperature <|> generator <|> inputCoordinates

----------------------------------------------------------------------------
popOnConfig :: Array ConfigurationPart -> Json -> Json
popOnConfig xs s = foldl go s xs
  where
  go s (Temperature x) = "temperature" := x ~> s

  go s (InputCoordinates name value) = updateInner "inputCoordinates" (\x -> name := value ~> x) s

  go s (Generator xs) = "generator" := (foldl writeGenerator jsonEmptyObject xs) ~> s

writeGenerator :: Json -> GeneratorPart -> Json
writeGenerator s (Add as) = updateInner "add" (\c -> foldl writeAdd c as) s

writeGenerator s (Box as) = updateInner "box" identity s

writeGenerator s (Parameters as) = updateInner "parameters" identity s

writeAdd :: Json -> GeneratorAddPart -> Json
writeAdd s (Density name units) = "density" := ("name" := name ~> "units" := units ~> jsonEmptyObject) ~> s

writeAdd s (Population _) = s

writeAdd s (Species _) = s

writeAdd s (BoxAction _) = s

writeAdd s (Rotate _) = s

writeAdd s (Positioning _) = s
