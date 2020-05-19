module Test.Main where

import Prelude

import Data.Homogeneous.Record (homogeneous, toRecord) as Record
import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main = do
  let
    i = Record.homogeneous { one: 1, two: 2, three: 3 }

    multiply = pure (_ * 2)

    o = multiply <*> i

  logShow (Record.toRecord o)
