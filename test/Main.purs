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

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Live Parser Spec" do
          describe "Example files" do
            describe "atomshake" do
              it "singlewater" do
                input <- loadDissolveFile "examples/atomshake/singlewater.txt"
                isRight input `shouldEqual` true
            describe "Accumulate" do
              it "accumulate" do
                input <- loadDissolveFile "examples/accumulate/accumulate.txt"
                isRight input `shouldEqual` true
            describe "Bragg" do
              it "intensities" do
                input <- loadDissolveFile "examples/bragg/intensities.txt"
                isRight input `shouldEqual` true
              it "intensities-111" do
                input <- loadDissolveFile "examples/bragg/intensities-111.txt"
                isRight input `shouldEqual` true
              it "mgo" do
                input <- loadDissolveFile "examples/bragg/mgo.txt"
                isRight input `shouldEqual` true
            describe "Broadening" do
              it "argon_dep0.1indep0.2.txt" do
                input <- loadDissolveFile "examples/broadening/argon_dep0.1indep0.2.txt"
                isRight input `shouldEqual` true
              it "argon_dep0.2indep0.1.txt" do
                input <- loadDissolveFile "examples/broadening/argon_dep0.2indep0.1.txt"
                isRight input `shouldEqual` true
              it "argon_qdep0.1.txt" do
                input <- loadDissolveFile "examples/broadening/argon_qdep0.1.txt"
                isRight input `shouldEqual` true
              it "argon_qdep0.2.txt" do
                input <- loadDissolveFile "examples/broadening/argon_qdep0.2.txt"
                isRight input `shouldEqual` true
              it "argon_qindep0.1.txt" do
                input <- loadDissolveFile "examples/broadening/argon_qindep0.1.txt"
                isRight input `shouldEqual` true
              it "argon_qindep0.2.txt" do
                input <- loadDissolveFile "examples/broadening/argon_qindep0.2.txt"
                isRight input `shouldEqual` true
