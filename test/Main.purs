module Test.Main where

import Prelude

import Data.Homogeneous.Record (homogeneous, toRecord) as Homogeneous.Record
import Effect (Effect)
import Effect.Console (logShow)

main :: Effect Unit
main = do
  let
    r = { one: 1, two: 2, three: 3 }

    multiply = pure (_ * 2)

    -- | We put a record in
    o = multiply <*> Homogeneous.Record.homogeneous r

    -- | We get a record back
    r' = Homogeneous.Record.toRecord o

  logShow r'
