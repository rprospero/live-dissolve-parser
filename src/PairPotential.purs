module PairPotential where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Util (bool, dissolveTokens, punt, signedFloat)

data PairPart
  = Range Number
  | Delta Number
  | ShortRangeTruncation String
  | IncludeCoulomb Boolean
  | Parameters (Array String)

derive instance genericPairPart :: Generic PairPart _

instance showPairPart :: Show PairPart where
  show x = genericShow x

range = dissolveTokens.symbol "Range" *> (Range <$> dissolveTokens.float)

delta = dissolveTokens.symbol "Delta" *> (Delta <$> dissolveTokens.float)

short = dissolveTokens.symbol "ShortRangeTruncation" *> (ShortRangeTruncation <$> dissolveTokens.identifier)

coulomb = dissolveTokens.symbol "IncludeCoulomb" *> (IncludeCoulomb <$> bool)

parameters = punt "Parameters" Parameters

pairPart = range <|> delta <|> short <|> coulomb <|> parameters
