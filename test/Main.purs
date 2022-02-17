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
            describe "calculate_avgmol" do
              parseTest "bendy" "calculate_avgmol"
              parseTest "water" "calculate_avgmol"
            describe "calculate_cn" do
              parseTest "cn" "calculate_cn"
            describe "calculate_dangle" do
              parseTest "dangle" "calculate_dangle"
            describe "calculate_rdf" do
              parseTest "rdf" "calculate_rdf"
              parseTest "npt" "calculate_rdf"
            describe "calculate_sdf" do
              parseTest "sdf" "calculate_sdf"
            describe "correlations" do
              parseTest "sq" "correlations"
            describe "energyforce1" do
              parseTest "water3000-coul" "energyforce1"
              parseTest "water3000-elec" "energyforce1"
              parseTest "water3000-full" "energyforce1"
              parseTest "water3000-intra" "energyforce1"
              parseTest "water3000-vdw" "energyforce1"
