module Force where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Foreign.Object
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data ForceInfo
  = Harmonic Number Number
  | Ref String

derive instance genericForceInfo :: Generic ForceInfo _

instance showForceInfo :: Show ForceInfo where
  show x = genericShow x

writeRef Nothing s = s

writeRef (Just (Harmonic k x)) s =
  "type" := "Harmonic"
    ~> "constant"
    := k
    ~> "length"
    := x
    ~> s

writeRef (Just (Ref name)) s =
  "type" := "Reference"
    ~> "master"
    := name
    ~> s
