module Main where

import Prelude

import Chapter2 (factorial)
import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main = do
    logShow (factorial 4)
