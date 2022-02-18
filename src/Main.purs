module Main where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Prelude
import Data.Either (Either(..))
import Dissolve
import Effect (Effect)
import Effect.Aff (runAff)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  _ <-
    runAff (const $ pure unit) do
      input ← loadDissolveFile "examples/energyforce3/py5-ntf2.txt"
      -- input ← loadDissolveFile "examples/accumulate/accumulate.txt"
      case input of
        Left x -> log $ show x
        Right x -> log $ stringify $ encodeJson $ asDissolve x
  pure unit
