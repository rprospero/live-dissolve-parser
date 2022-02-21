module PairPotential where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Force
import Foreign.Object
import Prelude
import Util
import Control.Alternative ((<|>))
import Data.Array (catMaybes, foldl, head, tail)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)

data PairPart
  = Range Number
  | Delta Number
  | ShortRangeTruncation String
  | IncludeCoulomb Boolean
  | Parameters String String Number ForceInfo
  | ManualChargeSource Boolean
  | ForceChargeSource Boolean
  | CoulombTruncation String

derive instance genericPairPart :: Generic PairPart _

instance showPairPart :: Show PairPart where
  show x = genericShow x

range = dissolveTokens.symbol "Range" *> (Range <$> signedNum)

delta = dissolveTokens.symbol "Delta" *> (Delta <$> dissolveTokens.float)

short = dissolveTokens.symbol "ShortRangeTruncation" *> (ShortRangeTruncation <$> dissolveTokens.identifier)

coulomb = dissolveTokens.symbol "IncludeCoulomb" *> (IncludeCoulomb <$> bool)

manualChargeSource = dissolveTokens.symbol "ManualChargeSource" *> (ManualChargeSource <$> bool)

forceChargeSource = dissolveTokens.symbol "ForceChargeSource" *> (ForceChargeSource <$> bool)

coulombTruncation = dissolveTokens.symbol "CoulombTruncation" *> (CoulombTruncation <$> dissolveTokens.identifier)

parameters = dissolveTokens.symbol "Parameters" *> (Parameters <$> allString <*> dissolveTokens.identifier <*> signedFloat <*> forceInfo)

pairPart = range <|> delta <|> short <|> coulomb <|> parameters <|> manualChargeSource <|> forceChargeSource <|> coulombTruncation

----------------------------------------------------------------------------
popOnPair :: Array PairPart -> Json -> Json
popOnPair xs s = foldl go s xs
  where
  go s (Range x) = "range" := x ~> s

  go s (Delta x) = "delta" := x ~> s

  go s (IncludeCoulomb x) = "includeCoulomb" := x ~> s

  go s (CoulombTruncation x) = "coulombTruncation" := x ~> s

  go s (ShortRangeTruncation x) = "shortRangeTruncation" := x ~> s

  go s (ManualChargeSource x) = "manualChargeSource" := x ~> s

  go s (ForceChargeSource x) = "forceChargeSource" := x ~> s

  go s (Parameters name elem charge ref) = updateInner "parameters" (\c -> name := value ~> c) s
    where
    value = "element" := elem ~> "charge" := charge ~> writeRef (pure ref) jsonEmptyObject
