module PairPotential where

import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Util (bool, dissolveTokens, signedFloat)

data PairPart
  = Range Number
  | Delta Number
  | ShortRangeTruncation String
  | IncludeCoulomb Boolean
  | Parameters String String Number String Number Number

derive instance genericPairPart :: Generic PairPart _

instance showPairPart :: Show PairPart where
  show x = genericShow x

range = dissolveTokens.symbol "Range" *> (Range <$> dissolveTokens.float)

delta = dissolveTokens.symbol "Delta" *> (Delta <$> dissolveTokens.float)

short = dissolveTokens.symbol "ShortRangeTruncation" *> (ShortRangeTruncation <$> dissolveTokens.identifier)

coulomb = dissolveTokens.symbol "IncludeCoulomb" *> (IncludeCoulomb <$> bool)

parameters = dissolveTokens.symbol "Parameters" *> (Parameters <$> dissolveTokens.stringLiteral <*> dissolveTokens.identifier <*> signedFloat <*> dissolveTokens.identifier <*> signedFloat <*> signedFloat)

pairPart = range <|> delta <|> short <|> coulomb <|> parameters
