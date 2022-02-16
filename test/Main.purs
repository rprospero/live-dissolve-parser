module Test.Main where

import Prelude
import Dissolve (loadDissolveFile)
import Data.Either (isRight)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Live Parser Spec" do
          describe "Example files" do
            it "singlewater" do
              input <- loadDissolveFile "examples/singlewater.txt"
              isRight input `shouldEqual` true
            it "accumulate" do
              input <- loadDissolveFile "examples/accumulate.txt"
              isRight input `shouldEqual` true
            it "intensities" do
              input <- loadDissolveFile "examples/intensities.txt"
              isRight input `shouldEqual` true
            pending "singlewater.txt"
