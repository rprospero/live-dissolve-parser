module Configuration where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Foreign.Object
import Prelude hiding (between)
import Util
import Control.Alternative ((<|>))
import Data.Array (cons, foldl, head, many, tail)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.String (noneOf, skipSpaces, char, string)
import Xml

data ConfigurationPart
  = Generator (Array GeneratorPart)
  | Temperature Number
  | SizeFactor Number
  | CellDivisionLength Number
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

density = dissolveTokens.symbol "Density" *> (Density <$> arbitrary <*> units)

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

temperature = dissolveTokens.symbol "Temperature" *> (Temperature <$> signedNum)

sizeFactor = dissolveTokens.symbol "SizeFactor" *> (SizeFactor <$> signedNum)

cellDivisionLength = dissolveTokens.symbol "CellDivisionLength" *> (CellDivisionLength <$> dissolveTokens.float)

inputCoordinates = do
  _ <- dissolveTokens.symbol "InputCoordinates"
  name <- dissolveTokens.identifier
  path <- many $ noneOf [ ' ', '\n', '\t' ]
  skipSpaces
  _ <- string "End"
  _ <- dissolveTokens.symbol "InputCoordinates"
  pure (InputCoordinates name $ fromCharArray path)

configurationPart = temperature <|> generator <|> inputCoordinates <|> sizeFactor <|> cellDivisionLength

----------------------------------------------------------------------------
popOnConfig :: Array ConfigurationPart -> Json -> Json
popOnConfig xs s = foldl go s xs
  where
  go s (Temperature x) = "temperature" := x ~> s

  go s (SizeFactor x) = "sizeFactor" := x ~> s

  go s (CellDivisionLength x) = "cellDivisionLength" := x ~> s

  go s (InputCoordinates name value) = updateInner "inputCoordinates" (\x -> name := value ~> x) s

  go s (Generator xs) = "generator" := (foldl writeGenerator jsonEmptyObject xs) ~> s

writeGenerator :: Json -> GeneratorPart -> Json
writeGenerator s (Add as) = updateArray "add" (\c -> fromArray $ cons (writeAdd as) c) s

writeGenerator s (Box bs) = updateInner "box" (writeBox bs) s

writeGenerator s (Parameters ps) = updateInner "parameters" (writeParam ps) s

writeAdd :: Array GeneratorAddPart -> Json
writeAdd = foldl go jsonEmptyObject
  where
  go s (Density name units) = "density" := ("name" := name ~> "units" := units ~> jsonEmptyObject) ~> s

  go s (Population x) = "population" := x ~> s

  go s (Species x) = "species" := x ~> s

  go s (BoxAction x) = "boxAction" := x ~> s

  go s (Rotate x) = "rotate" := x ~> s

  go s (Positioning x) = "positioning" := x ~> s

writeBox bs s = foldl go s bs
  where
  go s (Length x y z) = "length" := [ x, y, z ] ~> s

  go s (Angles x y z) = "angles" := [ x, y, z ] ~> s

  go s (NonPeriodic x) = "nonPeriodic" := x ~> s

writeParam ps s = foldl go s ps
  where
  go s (Param ps) =
    let
      name = maybe "undefined" identity $ head ps
    in
      name := tail ps ~> s

xmlOnConfig :: ConfigurationPart -> XmlNode -> XmlNode
xmlOnConfig (Temperature x) s = ("temperature" ::= x) s

xmlOnConfig (SizeFactor x) s = ("sizeFactor" ::= x) s

xmlOnConfig (CellDivisionLength x) s = ("cellDivisionLength" ::= x) s

xmlOnConfig (InputCoordinates name value) s = xmlActOn "inputCoordinates" [ "name" ::= name, "value" ::= value ] ::=> s

xmlOnConfig (Generator xs) s = xmlActOn "generator" (map xmlGenerator xs) ::=> s

xmlGenerator :: GeneratorPart -> XmlNode -> XmlNode
xmlGenerator (Add as) s = xmlActOn "add" (map go as) ::=> s
  where
  go (Density name units) c = xmlActOn "density" [ "name" ::= name, "units" ::= units ] ::=> c

  go (Population x) c = ("population" ::= x) c

  go (Species x) c = ("species" ::= x) c

  go (BoxAction x) c = ("boxAction" ::= x) c

  go (Rotate x) c = ("rotate" ::= x) c

  go (Positioning x) c = ("positioning" ::= x) c

xmlGenerator (Box bs) s = xmlActOn "box" (map go bs) ::=> s
  where
  go (Length x y z) c = xmlActOn "length" [ "x" ::= x, "y" ::= y, "z" ::= z ] ::=> c

  go (Angles x y z) c = xmlActOn "angles" [ "x" ::= x, "y" ::= y, "z" ::= z ] ::=> c

  go (NonPeriodic x) c = ("nonPeriodic" ::= x) c

xmlGenerator (Parameters ps) s = xmlActOn "parameters" (map go ps) ::=> s
  where
  go (Param ps) c =
    let
      name = maybe "undefined" identity $ head ps

      ts = maybe [] identity $ tail ps
    in
      ("name" ::= name $ (addTerms "parameter" ts)) ::=> c
