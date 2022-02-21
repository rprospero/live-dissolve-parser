module Force where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Foreign.Object
import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser.String (char)
import Util (MyParser, arbitrary, bool, dissolveTokens, namedContainer, punt, signedFloat, updateArray, updateInner)

data ForceInfo
  = Harmonic Number Number
  | Ref String
  | None

derive instance genericForceInfo :: Generic ForceInfo _

instance showForceInfo :: Show ForceInfo where
  show x = genericShow x

forceInfo = harmonic <|> ref <|> none
  where
  harmonic = dissolveTokens.symbol "Harmonic" *> (Harmonic <$> signedFloat <*> signedFloat)

  none = dissolveTokens.symbol "None" *> pure None

  ref = do
    _ <- char '@'
    value <- arbitrary
    dissolveTokens.whiteSpace
    pure $ Ref value

writeRef Nothing s = s

writeRef (Just None) s = s

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
