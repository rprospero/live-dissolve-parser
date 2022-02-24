module Master where

import Control.Monad.State
import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Array
import Data.Lens
import Force
import Prelude
import Control.Alternative ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Text.Parsing.Parser.Combinators (optionMaybe)
import Util (arbitrary, dissolveTokens, updateArray)
import Xml

data MasterPart
  = Angle String (Maybe ForceInfo)
  | Bond String (Maybe ForceInfo)
  | Torsion String (Maybe ForceInfo)
  | Improper String (Maybe ForceInfo)

derive instance genericMasterPart :: Generic MasterPart _

instance showMasterPart :: Show MasterPart where
  show x = genericShow x

angle = (dissolveTokens.symbol) "Angle" *> (Angle <$> (arbitrary <* dissolveTokens.whiteSpace) <*> optionMaybe forceInfo)

bond = (dissolveTokens.symbol) "Bond" *> (Bond <$> (arbitrary <* dissolveTokens.whiteSpace) <*> optionMaybe forceInfo)

torsion = (dissolveTokens.symbol) "Torsion" *> (Torsion <$> (arbitrary <* dissolveTokens.whiteSpace) <*> optionMaybe forceInfo)

improper = (dissolveTokens.symbol) "Improper" *> (Improper <$> (arbitrary <* dissolveTokens.whiteSpace) <*> optionMaybe forceInfo)

masterPart = angle <|> bond <|> torsion <|> improper

------- Start Json build
popOnMaster :: Array MasterPart -> Json -> Json
popOnMaster xs s = foldl go s xs
  where
  go s (Bond name ref) = updateArray "bonds" (\c -> fromArray $ flip snoc (writeMaster name ref) c) s

  go s (Angle name ref) = updateArray "angles" (\c -> fromArray $ flip snoc (writeMaster name ref) c) s

  go s (Torsion name ref) = updateArray "torsions" (\c -> fromArray $ flip snoc (writeMaster name ref) c) s

  go s (Improper name ref) = updateArray "improper" (\c -> fromArray $ flip snoc (writeMaster name ref) c) s

writeMaster name ref = "name" := name ~> writeRef ref jsonEmptyObject

xmlOnMaster :: MasterPart -> State XmlNode Unit
-- xmlOnMaster (Bond name ref) = xmlActOn "bond" [ "name" ::= name, xmlRef ref ] ::=> s
xmlOnMaster (Bond name ref) = onNewChild "bond" $ (xmlName .= name) *> xmlRef ref

xmlOnMaster (Angle name ref) = onNewChild "angle" $ (xmlName .= name) *> xmlRef ref

xmlOnMaster (Torsion name ref) = onNewChild "torsion" $ (xmlName .= name) *> xmlRef ref

xmlOnMaster (Improper name ref) = onNewChild "improper" $ (xmlName .= name) *> xmlRef ref
