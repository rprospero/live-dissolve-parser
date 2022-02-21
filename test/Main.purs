module Test.Main where

import Prelude
import Data.Either (isRight)
import Dissolve (loadDissolveFile)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it, pending)
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
            describe "energyforce2" do
              parseTest "full" "energyforce2"
              parseTest "one" "energyforce2"
              parseTest "two" "energyforce2"
              parseTest "torsions" "energyforce2"
            describe "energyforce3" do
              parseTest "poe" "energyforce3"
              parseTest "py4oh-ntf2" "energyforce3"
              parseTest "py5-ntf2" "energyforce3"
            describe "energyforce4" do
              parseTest "py4oh-ntf2" "energyforce4"
              parseTest "py5-ntf2" "energyforce4"
            describe "epsr" do
              parseTest "benzene" "epsr"
              parseTest "pcof" "epsr"
              parseTest "water-neutron-xray" "epsr"
              parseTest "water-poisson" "epsr"
            describe "exchangeable" do
              parseTest "watermeth" "exchangeable"
            describe "ff" do
              parseTest "kulmala2010" "ff"
              parseTest "ludwig-py5" "ff"
              parseTest "oplsaa2005-alcohols" "ff"
              parseTest "oplsaa2005-alkanes" "ff"
              parseTest "oplsaa2005-aromatics" "ff"
              parseTest "pcl2019-anions" "ff"
              parseTest "pcl2019-cations" "ff"
              parseTest "spcfw" "ff"
              parseTest "uff-nmethylformamide" "ff"
              pending "uff4mof-mof5" -- Works, but uses too much memory
            describe "inputs" do
              parseTest "benzene" "inputs"
              parseTest "py5-ntf2" "inputs"
              parseTest "water" "inputs"
            describe "md" do
              parseTest "benzene" "md"
            describe "molshake" do
              parseTest "benzene" "molshake"
            describe "rdfmethod" do
              parseTest "cells" "rdfmethod"
              parseTest "simple" "rdfmethod"
            describe "restart" do
              parseTest "benzene" "restart"
              parseTest "benzene2" "restart"
              parseTest "rdf" "restart"
            describe "xray" do
              parseTest "water" "xray"
