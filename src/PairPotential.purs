module PairPotential where

import Prelude
import Control.Alternative ((<|>))
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Array (catMaybes, foldl, head, tail)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Foreign.Object
import Util (bool, dissolveTokens, punt, signedFloat)

data PairPart
  = Range Number
  | Delta Number
  | ShortRangeTruncation String
  | IncludeCoulomb Boolean
  | Parameters (Array String)
  | ManualChargeSource Boolean
  | ForceChargeSource Boolean
  | CoulombTruncation String

derive instance genericPairPart :: Generic PairPart _

instance showPairPart :: Show PairPart where
  show x = genericShow x

range = dissolveTokens.symbol "Range" *> (Range <$> dissolveTokens.float)

delta = dissolveTokens.symbol "Delta" *> (Delta <$> dissolveTokens.float)

short = dissolveTokens.symbol "ShortRangeTruncation" *> (ShortRangeTruncation <$> dissolveTokens.identifier)

coulomb = dissolveTokens.symbol "IncludeCoulomb" *> (IncludeCoulomb <$> bool)

manualChargeSource = dissolveTokens.symbol "ManualChargeSource" *> (ManualChargeSource <$> bool)

forceChargeSource = dissolveTokens.symbol "ForceChargeSource" *> (ForceChargeSource <$> bool)

coulombTruncation = dissolveTokens.symbol "CoulombTruncation" *> (CoulombTruncation <$> dissolveTokens.identifier)

parameters = punt "Parameters" Parameters

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

  go s (Parameters xs) = caseJsonObject s (insertParam xs) s

  insertParam :: Array String -> Object Json -> Json
  insertParam xs ob = case head xs of
    Nothing -> fromObject ob
    Just name ->
      let
        ps = maybe jsonEmptyObject identity $ lookup "parameters" ob
      in
        "parameters" := (name := tail xs ~> ps) ~> fromObject ob
