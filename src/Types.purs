module Types where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Foreign.Object
import Master
import PairPotential
import Prelude
import Species
import Configuration
import Control.Monad.State (execState, modify)
import Data.Array (catMaybes, foldl, head, tail)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Layer (LayerPart)

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
          _ <- modify (writeConfig config)
          _ <- modify (\c -> "layer" := "Layer" ~> c)
          _ <- modify (writeSpecies species)
          _ <- modify (writePair pair)
          _ <- modify (writeMaster master)
          pure 7

writeSpecies :: (Array (Tuple String (Array SpeciesPart))) -> Json -> Json
writeSpecies xs s = "species" := foldl go jsonEmptyObject xs ~> s
  where
  go state (Tuple name xs) = name := (popOnSpecies xs jsonEmptyObject) ~> state

writeConfig :: (Array (Tuple String (Array ConfigurationPart))) -> Json -> Json
writeConfig xs s = "configuration" := foldl go jsonEmptyObject xs ~> s
  where
  go state (Tuple name xs) = name := (popOnConfig xs jsonEmptyObject) ~> state

writePair :: Maybe (Array PairPart) -> Json -> Json
writePair Nothing s = s

writePair (Just xs) s = "PairPotentials" := (popOnPair xs jsonEmptyObject) ~> s

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
