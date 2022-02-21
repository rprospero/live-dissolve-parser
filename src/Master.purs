module Master where

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Array
import Force
import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Util (arbitrary, dissolveTokens, punt, signedFloat, updateArray)

data MasterPart
  = Angle String (Maybe ForceInfo)
  | Bond String (Maybe ForceInfo)
  | Torsion String (Maybe ForceInfo)
  | Improper (Array String)

derive instance genericMasterPart :: Generic MasterPart _

instance showMasterPart :: Show MasterPart where
  show x = genericShow x

angle = (dissolveTokens.symbol) "Angle" *> (Angle <$> (arbitrary <* dissolveTokens.whiteSpace) <*> optionMaybe forceInfo)

bond = (dissolveTokens.symbol) "Bond" *> (Bond <$> (arbitrary <* dissolveTokens.whiteSpace) <*> optionMaybe forceInfo)

torsion = (dissolveTokens.symbol) "Torsion" *> (Torsion <$> (arbitrary <* dissolveTokens.whiteSpace) <*> optionMaybe forceInfo)

improper = punt "Improper" Improper

masterPart = angle <|> bond <|> torsion <|> improper

------- Start Json build
popOnMaster :: Array MasterPart -> Json -> Json
popOnMaster xs s = foldl go s xs
  where
  go s (Bond name ref) = updateArray "bonds" (\c -> fromArray $ flip snoc (writeMaster name ref) c) s

  go s (Angle name ref) = updateArray "angles" (\c -> fromArray $ flip snoc (writeMaster name ref) c) s

  go s (Torsion name ref) = updateArray "torsions" (\c -> fromArray $ flip snoc (writeMaster name ref) c) s

  go s (Improper xs) = updateArray "improper" (\c -> fromArray c) s

writeMaster name ref = "name" := name ~> writeRef ref jsonEmptyObject
