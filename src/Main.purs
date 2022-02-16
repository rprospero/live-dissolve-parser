module Main where

import Prelude
import Dissolve (loadDissolveFile)
import Effect (Effect)
import Effect.Aff (runAff)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  _ <-
    runAff (const $ pure unit) do
      input ← loadDissolveFile "examples/argon_dep0.1indep0.2.txt"
      log $ show input
  pure unit
