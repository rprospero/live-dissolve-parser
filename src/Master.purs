module Master where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Util (dissolveTokens, signedFloat)

data MasterPart
  = Angle String String Number Number
  | Bond String String Number Number

derive instance genericMasterPart :: Generic MasterPart _

instance showMasterPart :: Show MasterPart where
  show x = genericShow x

angle = dissolveTokens.symbol "Angle" *> (Angle <$> dissolveTokens.identifier <*> dissolveTokens.identifier <*> dissolveTokens.float <*> dissolveTokens.float)

bond = dissolveTokens.symbol "Bond" *> (Bond <$> dissolveTokens.identifier <*> dissolveTokens.identifier <*> dissolveTokens.float <*> dissolveTokens.float)

masterPart = angle <|> bond
