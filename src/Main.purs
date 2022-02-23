module Main where

import Data.Tuple
import Prelude
import Web.File.Blob
import Web.File.Url
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.MediaType.Common (applicationJSON, applicationXML)
import Dissolve (asDissolve, dissolve)
import Effect (Effect)
import Halogen (liftEffect, put)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Text.Parsing.Parser (runParser)
import Xml (toXml, xmlEncode)

type State
  = Either String (Tuple String String)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body ← HA.awaitBody
    runUI component unit body

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = Left "No Input"

  render ∷ ∀ t0. State → HH.HTML t0 String
  render s =
    HH.div [ HP.class_ $ H.ClassName "master" ]
      [ HH.div_
          [ HH.label [ HP.for "source" ] [ HH.text "Dissolve Input File" ]
          , HH.textarea [ HP.id "source", HE.onValueInput identity ]
          ]
      , HH.div_
          [ case s of
              Left x -> HH.div_ [ HH.text (show x) ]
              Right (Tuple xml json) ->
                HH.ul_
                  [ HH.li_ [ HH.a [ HP.href xml ] [ HH.text "Xml" ] ]
                  , HH.li_ [ HH.a [ HP.href json ] [ HH.text "Json" ] ]
                  ]
          ]
      ]

  handleAction s = do
    case runParser s dissolve of
      Left x -> put (Left $ show x)
      Right x -> do
        xml <- liftEffect $ createObjectURL (fromString (xmlEncode $ toXml $ asDissolve x) applicationJSON)
        json <- liftEffect $ createObjectURL (fromString (stringify $ encodeJson $ asDissolve x) applicationXML)
        put (Right (Tuple xml json))
