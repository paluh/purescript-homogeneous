module Test.Main where

import Prelude

import Control.Comonad (extract, (=>>))
import Control.Semigroupoid (composeFlipped)
import Data.Homogeneous.Record (homogeneous, toRecord) as Homogeneous.Record
import Data.Homogeneous.Variant (homogeneous, toVariant) as Homogeneous.Variant
import Data.Variant (Variant, case_, on)
import Data.Variant (inj) as Variant
import Effect (Effect)
import Effect.Console (logShow)
import Type.Prelude (SProxy(..))

main :: Effect Unit
main = do
  let
    ir = { one: 1, two: 2, three: 3 }

    multiply = pure (_ * 2)

    -- | We put a record in
    or = multiply <*> Homogeneous.Record.homogeneous ir

    -- | We get a record back
    or' = Homogeneous.Record.toRecord or

  logShow or'

  let
    mul' = composeFlipped Homogeneous.Variant.toVariant $ case_
      # on (SProxy ∷ SProxy "one") (mul 1)
      # on (SProxy ∷ SProxy "two") (mul 2)

    o = Homogeneous.Variant.homogeneous ((Variant.inj (SProxy ∷ SProxy "one") 1) ∷ Variant (two ∷ Int, one ∷ Int))
    t = Homogeneous.Variant.homogeneous ((Variant.inj (SProxy ∷ SProxy "two") 2) ∷ Variant (two ∷ Int, one ∷ Int))

    t' = t =>> mul'
    o' = o =>> mul'

  logShow $ extract t'
  logShow $ extract o'
