module Test.Chapter2 where

import Prelude

import Chapter2 (factorial, fib, findFirst)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)


-- spago test --main Test.Chapter2

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
    describe "Chapter2" do
        it "should calculate factorial" do
            factorial 4 `shouldEqual` 24
        it "should calculate nth fib" do
            -- 0, 1, 1, 2, 3, 5, 8 ...
            fib 3 `shouldEqual` 2
            fib 6 `shouldEqual` 8
        it "should return first elem matching criteria" do
            findFirst [1, 2, 3] (\x -> fromMaybe 0 x == 2)

