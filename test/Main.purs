module Test.Main where

import Prelude
import Dissolve (loadDissolveFile)
import Data.Either (isRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

parseTest name path =
  it name do
    input <- loadDissolveFile $ "examples/" <> path <> "/" <> name <> ".txt"
    isRight input `shouldEqual` true

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Live Parser Spec" do
          describe "Example files" do
            describe "atomshake" do
              parseTest "singlewater" "atomshake"
            describe "Accumulate" do
              parseTest "accumulate" "accumulate"
            describe "Bragg" do
              parseTest "intensities" "bragg"
              parseTest "intensities-111" "bragg"
              parseTest "mgo" "bragg"
            describe "Broadening" do
              parseTest "argon_dep0.1indep0.2" "broadening"
              parseTest "argon_dep0.2indep0.1" "broadening"
              parseTest "argon_qdep0.1" "broadening"
              parseTest "argon_qdep0.2" "broadening"
              parseTest "argon_qindep0.1" "broadening"
              parseTest "argon_qindep0.2" "broadening"
