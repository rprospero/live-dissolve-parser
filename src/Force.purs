module Force where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Foreign.Object
import Prelude
import Control.Alternative ((<|>))
import Data.Array (many)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser.Combinators (sepBy, sepBy1, (<?>))
import Text.Parsing.Parser.String (char)
import Util (MyParser, arbitrary, bool, dissolveTokens, named, namedContainer, punt, signedFloat, updateArray, updateInner)

data ForceInfo
  = Harmonic Number Number
  | Ref String
  | Cos3 Number Number Number
  | Cos Number Number Number Number
  | CosN (Array Number)
  | CosNC (Array Number)
  | LJ Number Number
  | LJGeometric Number Number
  | None

derive instance genericForceInfo :: Generic ForceInfo _

instance showForceInfo :: Show ForceInfo where
  show x = genericShow x

forceInfo = harmonic <|> ref <|> none <|> cos3 <|> cosNC <|> cosN <|> cos <|> ljGeometric <|> lj <?> "Force Info"
  where
  harmonic = dissolveTokens.symbol "Harmonic" *> (Harmonic <$> named signedFloat <*> named signedFloat)

  cos3 = dissolveTokens.symbol "Cos3" *> (Cos3 <$> signedFloat <*> signedFloat <*> signedFloat)

  cosNC = dissolveTokens.symbol "CosNC" *> ((CosNC <<< toUnfoldable) <$> (sepBy1 signedFloat dissolveTokens.whiteSpace))

  cosN = dissolveTokens.symbol "CosN" *> ((CosN <<< toUnfoldable) <$> (sepBy1 signedFloat dissolveTokens.whiteSpace))

  cos = dissolveTokens.symbol "Cos" *> (Cos <$> signedFloat <*> signedFloat <*> signedFloat <*> signedFloat)

  lj = dissolveTokens.symbol "LJ" *> (LJ <$> named signedFloat <*> named signedFloat)

  ljGeometric = dissolveTokens.symbol "LJGeometric" *> (LJGeometric <$> signedFloat <*> signedFloat)

  none = dissolveTokens.symbol "None" *> pure None

  ref = do
    _ <- char '@'
    value <- arbitrary
    dissolveTokens.whiteSpace
    pure $ Ref value

writeRef Nothing s = s

writeRef (Just None) s = s

writeRef (Just (Harmonic k eq)) s =
  "type" := "Harmonic"
    ~> "length"
    := eq
    ~> "constant"
    := k
    ~> s

writeRef (Just (Cos i j k l)) s =
  "type" := "Cos"
    ~> "l"
    := l
    ~> "k"
    := k
    ~> "j"
    := j
    ~> "i"
    := i
    ~> s

writeRef (Just (CosNC xs)) s =
  "type" := "CosNC"
    ~> "terms"
    := xs
    ~> s

writeRef (Just (CosN xs)) s =
  "type" := "CosN"
    ~> "terms"
    := xs
    ~> s

writeRef (Just (LJ a b)) s =
  "type" := "LJ"
    ~> "a"
    := a
    ~> "b"
    := b
    ~> s

writeRef (Just (LJGeometric a b)) s =
  "type" := "LJGeometric"
    ~> "a"
    := a
    ~> "b"
    := b
    ~> s

writeRef (Just (Cos3 i j k)) s =
  "type" := "Cos3"
    ~> "k"
    := k
    ~> "j"
    := j
    ~> "i"
    := i
    ~> s

writeRef (Just (Ref name)) s =
  "type" := "Reference"
    ~> "master"
    := name
    ~> s
