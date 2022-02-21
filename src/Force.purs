module Force where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Foreign.Object
import Prelude
import Xml
import Control.Alternative ((<|>))
import Data.Array (foldl)
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
  | Cos3 Number Number Number
  | Cos Number Number Number Number
  | CosN (Array Number)
  | CosNC (Array Number)
  | LJ Number Number
  | LJGeometric (Array Number)
  | None

derive instance genericForceInfo :: Generic ForceInfo _

instance showForceInfo :: Show ForceInfo where
  show x = genericShow x

forceInfo = harmonic <|> ref <|> none <|> cos3 <|> cosNC <|> cosN <|> cos <|> ljGeometric <|> lj <?> "Force Info"
  where
  harmonic = dissolveTokens.symbol "Harmonic" *> (Harmonic <$> named signedNum <*> named signedNum)

  cos3 = dissolveTokens.symbol "Cos3" *> (Cos3 <$> named signedNum <*> named signedNum <*> named signedNum)

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

writeRef (Just (Ref name)) s =
  "type" := "Reference"
    ~> "master"
    := name
    ~> s

xmlRef :: (Maybe ForceInfo) -> XmlNode -> XmlNode
xmlRef Nothing s = s

xmlRef (Just (Ref name)) s = ("master" ::= name $ xmlEmptyNode "reference") ::=> s

xmlRef (Just (Harmonic k eq)) s = (("eq" ::= eq) <<< ("k" ::= k) $ xmlEmptyNode "harmonic") ::=> s

xmlRef (Just (Cos i j k l)) s = (("l" ::= l) <<< ("k" ::= k) <<< ("j" ::= j) <<< ("i" ::= i) $ xmlEmptyNode "cos") ::=> s

xmlRef (Just (Cos3 i j k)) s = (("k" ::= k) <<< ("j" ::= j) <<< ("i" ::= i) $ xmlEmptyNode "cos3") ::=> s

xmlRef (Just (LJ a b)) s = (("b" ::= b) <<< ("a" ::= a) $ xmlEmptyNode "lj") ::=> s

xmlRef (Just (CosN cs)) s = (foldl go (xmlEmptyNode "CosN") cs) ::=> s
  where
  go state term = (("value" ::= term) $ xmlEmptyNode "term") ::=> state

xmlRef (Just (CosNC cs)) s = (foldl go (xmlEmptyNode "CosNC") cs) ::=> s
  where
  go state term = (("value" ::= term) $ xmlEmptyNode "term") ::=> state

xmlRef (Just (LJGeometric cs)) s = (foldl go (xmlEmptyNode "LJGeometric") cs) ::=> s
  where
  go state term = (("value" ::= term) $ xmlEmptyNode "term") ::=> state

xmlRef (Just None) s = s
