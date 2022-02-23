module CLI where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Dissolve
import Prelude
import Data.Array ((!!))
import Data.Maybe
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff)
import Effect.Class.Console (log)
import Node.Process (argv)
import Xml

main :: Effect Unit
main = do
  args <- argv
  _ <-
    runAff (const $ pure unit) do
      input ← loadDissolveFile $ maybe "examples/energyforce3/py5-ntf2.txt" identity $ args !! 2
      -- input ← loadDissolveFile "examples/accumulate/accumulate.txt"
      case input of
        Left x -> log $ show x
        Right x -> log $ stringify $ encodeJson $ asDissolve x
        -- Right x -> log $ xmlEncode $ toXml $ asDissolve x
  pure unit
