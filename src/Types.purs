module Types where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Master
import Prelude
import Configuration (ConfigurationPart)
import Control.Monad.State (execState, modify)
import Data.Array (catMaybes, head)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Layer (LayerPart)
import PairPotential (PairPart)
import Species (SpeciesPart)

data Section
  = Species String (Array SpeciesPart)
  | Configuration String (Array ConfigurationPart)
  | PairPotentials (Array PairPart)
  | Layer String (Array LayerPart)
  | Master (Array MasterPart)

derive instance genericSection :: Generic Section _

instance showSection :: Show Section where
  show x = genericShow x

data Dissolve
  = Dissolve (Maybe (Array MasterPart)) (Array (Tuple String (Array ConfigurationPart))) (Array (Tuple String (Array LayerPart))) (Maybe (Array PairPart)) (Array (Tuple String (Array SpeciesPart)))

instance encodeDissolve :: EncodeJson Dissolve where
  encodeJson (Dissolve master config layer pair species) =
    flip execState jsonEmptyObject
      $ do
          _ <- modify (\c -> "config" := "Config" ~> c)
          _ <- modify (\c -> "layer" := "Layer" ~> c)
          _ <- modify (\c -> "pair" := "pair" ~> c)
          _ <- modify (\c -> "species" := "species" ~> c)
          _ <- modify (writeMaster master)
          pure 7

writeMaster Nothing s = s

writeMaster (Just xs) s =
  "Master"
    := ( "impropers"
          := (catMaybes $ map getImproper $ xs)
          ~> "torsions"
          := (catMaybes $ map getTorsion $ xs)
          ~> "bond"
          := (catMaybes $ map getBond $ xs)
          ~> "angle"
          := (catMaybes $ map getAngle $ xs)
          ~> jsonEmptyObject
      )
    ~> s

-- "contents" := fromArray (map encodeJson xs) ~> jsonEmptyObject
