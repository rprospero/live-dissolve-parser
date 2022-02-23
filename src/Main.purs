module Main where

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Dissolve (asDissolve, dissolve)
import Effect (Effect)
import Halogen (Component, put)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Text.Parsing.Parser (runParser)
import Xml (toXml, xmlEncode)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body ← HA.awaitBody
    runUI component unit body

component :: forall t59 t60 t75 t78. Component t75 t78 t59 t60
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = ""

  render ∷ ∀ t0. String → HH.HTML t0 String
  render s =
    let
      parsed = runParser s dissolve
    in
      HH.div [ HP.class_ $ H.ClassName "master" ]
        [ case parsed of
            Left x -> HH.text (show x)
            Right x -> HH.text (xmlEncode $ toXml $ asDissolve x)
        , HH.textarea [ HP.id "source", HE.onValueInput identity, HP.value s ]
        , case parsed of
            Left x -> HH.text (show x)
            Right x -> HH.text (stringify $ encodeJson $ asDissolve x)
        ]

  handleAction = put

-- input ← loadDissolveFile $ maybe "examples/energyforce3/py5-ntf2.txt" identity $ args !! 2
-- case input of
--   Left x -> log $ show x
--   Right x -> log $ stringify $ encodeJson $ asDissolve x
-- Right x -> log $ xmlEncode $ toXml $ asDissolve x
