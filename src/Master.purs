module Master where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Util (dissolveTokens, punt, signedFloat)

data MasterPart
  = Angle (Array String)
  | Bond (Array String)
  | Torsion (Array String)
  | Improper (Array String)

getAngle (Angle xs) = Just xs

getAngle _ = Nothing

getBond (Bond xs) = Just xs

getBond _ = Nothing

getTorsion (Torsion xs) = Just xs

getTorsion _ = Nothing

getImproper (Improper xs) = Just xs

getImproper _ = Nothing

derive instance genericMasterPart :: Generic MasterPart _

instance showMasterPart :: Show MasterPart where
  show x = genericShow x

angle = punt "Angle" Angle

bond = punt "Bond" Bond

torsion = punt "Torsion" Torsion

improper = punt "Improper" Improper

masterPart = angle <|> bond <|> torsion <|> improper

------- Start Json build
instance encodeMasterPart :: EncodeJson MasterPart where
  encodeJson (Angle xs) = "tag" := "Angle" ~> "contents" := fromArray (map fromString xs) ~> jsonEmptyObject
  encodeJson (Bond xs) = "tag" := "Bond" ~> "contents" := fromArray (map fromString xs) ~> jsonEmptyObject
  encodeJson (Torsion xs) = "tag" := "Torsion" ~> "contents" := fromArray (map fromString xs) ~> jsonEmptyObject
  encodeJson (Improper xs) = "tag" := "Improper" ~> "contents" := fromArray (map fromString xs) ~> jsonEmptyObject
