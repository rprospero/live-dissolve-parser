module Master where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Util (dissolveTokens, punt, signedFloat)

data MasterPart
  = Angle (Array String)
  | Bond (Array String)
  | Torsion (Array String)
  | Improper (Array String)

derive instance genericMasterPart :: Generic MasterPart _

instance showMasterPart :: Show MasterPart where
  show x = genericShow x

angle = punt "Angle" Angle

bond = punt "Bond" Bond

torsion = punt "Torsion" Torsion

improper = punt "Improper" Improper

masterPart = angle <|> bond <|> torsion <|> improper
