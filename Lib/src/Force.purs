module Force where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Lens
import Data.Lens.At
import Foreign.Object
import Prelude
import Xml
import Control.Alternative ((<|>))
import Control.Monad.State (State)
import Data.Array (foldl)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.List.NonEmpty (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser.Combinators (sepBy1, (<?>))
import Text.Parsing.Parser.String (char)
import Util (arbitrary, dissolveTokens, named, signedFloat, signedNum)

data ForceInfo
  = Harmonic Number Number
  | Ref String
  | Cos2 Number Number Number Number
  | Cos3 Number Number Number
  | Cos Number Number Number Number
  | CosN (Array Number)
  | CosNC (Array Number)
  | UFFCosine Number Number Number
  | LJ Number Number
  | LJGeometric (Array Number)
  | None

derive instance genericForceInfo :: Generic ForceInfo _

instance showForceInfo :: Show ForceInfo where
  show x = genericShow x

forceInfo = harmonic <|> ref <|> none <|> cos2 <|> cos3 <|> cosNC <|> cosN <|> cos <|> uffCosine <|> ljGeometric <|> lj <?> "Force Info"
  where
  harmonic = dissolveTokens.symbol "Harmonic" *> (Harmonic <$> named signedNum <*> named signedNum)

  cos2 = dissolveTokens.symbol "Cos2" *> (Cos2 <$> named signedNum <*> named signedNum <*> named signedNum <*> named signedNum)
  cos3 = dissolveTokens.symbol "Cos3" *> (Cos3 <$> named signedNum <*> named signedNum <*> named signedNum)
  uffCosine = dissolveTokens.symbol "UFFCosine" *> (UFFCosine <$> named signedNum <*> named signedNum <*> named signedNum)

  cosNC = dissolveTokens.symbol "CosNC" *> ((CosNC <<< toUnfoldable) <$> (sepBy1 signedFloat dissolveTokens.whiteSpace))

  cosN = dissolveTokens.symbol "CosN" *> ((CosN <<< toUnfoldable) <$> (sepBy1 signedFloat dissolveTokens.whiteSpace))

  cos = dissolveTokens.symbol "Cos" *> (Cos <$> signedFloat <*> signedFloat <*> signedFloat <*> signedFloat)

  lj = dissolveTokens.symbol "LJ" *> (LJ <$> named signedFloat <*> named signedFloat)

  ljGeometric = dissolveTokens.symbol "LJGeometric" *> ((LJGeometric <<< toUnfoldable) <$> (sepBy1 signedNum dissolveTokens.whiteSpace))

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

writeRef (Just (LJGeometric terms)) s =
  "type" := "LJGeometric"
    ~> "terms"
    := terms
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

writeRef (Just (UFFCosine i j k)) s =
  "type" := "UFFCosine"
    ~> "k"
    := k
    ~> "j"
    := j
    ~> "i"
    := i
    ~> s

writeRef (Just (Cos2 i j k l)) s =
  "type" := "Cos2"
    ~> "l"
    := l
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

xmlRef :: (Maybe ForceInfo) -> State XmlNode Unit
xmlRef Nothing = pure unit

xmlRef (Just None) = pure unit

xmlRef (Just (Ref name)) = onNewChild "reference" $ onAttr "master" name

xmlRef (Just (Harmonic k eq)) = onNewChild "harmonic" $ onAttr "eq" eq *> onAttr "k" k

xmlRef (Just (Cos i j k l)) = onNewChild "cos" $ onAttr "i" i *> onAttr "j" j *> onAttr "k" k *> onAttr "l" l
xmlRef (Just (Cos2 i j k l)) = onNewChild "cos3" $ onAttr "i" i *> onAttr "j" j *> onAttr "k" k *> onAttr "l" l

xmlRef (Just (Cos3 i j k)) = onNewChild "cos3" $ onAttr "i" i *> onAttr "j" j *> onAttr "k" k

xmlRef (Just (UFFCosine i j k)) = onNewChild "uffCosine" $ onAttr "i" i *> onAttr "j" j *> onAttr "k" k

xmlRef (Just (LJ a b)) = onNewChild "lj" $ onAttr "a" a *> onAttr "b" b

xmlRef (Just (CosN cs)) = onNewChild "CosN" $ for_ cs (onNewChild "term" <<< onAttr "value")

xmlRef (Just (CosNC cs)) = onNewChild "CosNC" $ for_ cs (onNewChild "term" <<< onAttr "value")

xmlRef (Just (LJGeometric cs)) = onNewChild "LJGeometric" $ for_ cs (onNewChild "term" <<< onAttr "value")
